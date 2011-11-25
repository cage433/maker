package starling.booter;

import javax.swing.*;
import java.io.*;
import java.lang.management.ManagementFactory;
import java.lang.reflect.Method;
import java.net.*;
import java.security.*;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Downloads
 */
class Booter {
    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            System.out.println("Usage: java -jar booter.jar <appurl> <appname>");
            Thread.sleep(5000);
            System.exit(1);
        }

        URL appBaseURL = new URL(args[0]);
        String appName = args[1];
        File tmp = new File(System.getProperty("java.io.tmpdir"));
        File parentCacheDir = new File(tmp, "starling-cache");
        if (!parentCacheDir.exists()) parentCacheDir.mkdir();

        String cacheDirName = appName + "-" +
                appBaseURL.getHost() +
                (appBaseURL.getPort() == 80 ? "" : ("-" + appBaseURL.getPort())) +
                (appBaseURL.getPath().equals("") ? "" : "-" + appBaseURL.getPath());

        File cacheDir = new File(parentCacheDir, cacheDirName.replaceAll("/", "-"));
        if (!cacheDir.exists()) cacheDir.mkdir();
        File logFile = new File(cacheDir, "log.txt");
        System.setProperty("stdout.logfile", logFile.getPath());
        System.setOut(new java.io.PrintStream(new TeeOutputStream(System.out, new FileOutputStream(logFile))));
        System.setErr(new java.io.PrintStream(new TeeOutputStream(System.err, new FileOutputStream(logFile))));
        Proxy proxy = Proxy.NO_PROXY;//detectClosestHost(appBaseURL);

        System.out.println("Cachdir: " + cacheDir.getAbsolutePath());
        run(cacheDir, proxy, appBaseURL, Arrays.copyOfRange(args, 2, args.length));
    }

//   Takes about 1second
//    private static Proxy detectClosestHost(URL appBaseURL) throws Exception {
//        final LinkedBlockingQueue<Proxy> proxyList = new LinkedBlockingQueue<Proxy>();
//        final InetAddress[] hosts = { InetAddress.getByName(appBaseURL.getHost()), InetAddress.getByName("172.20.15.205") };
//        final Proxy[] proxies = { Proxy.NO_PROXY, new Proxy(Proxy.Type.HTTP, new InetSocketAddress("172.20.15.205", 3128)) };
//        for (int i = 0; i < hosts.length; i++) {
//            final InetAddress host = hosts[i];
//            final Proxy proxy = proxies[i];
//            new Thread(new Runnable() { public void run() {
//                try {
//                    long start = System.nanoTime();
//                    long startX = System.currentTimeMillis();
//                    boolean reachable = host.isReachable(2000);
//                    if (reachable) {
//                        long pingTime = System.nanoTime() - start;
//                        long pingTimeX = System.currentTimeMillis() - startX;
//                        System.out.println("Ping time for " + host + " " + pingTime + " " + pingTimeX);
//                        proxyList.put(proxy);
//                    }
//                } catch (Exception e) {
//                    System.out.println("Ping to " + host + " failed:" + e);
//                }
//            } }).start();
//        }
//        return proxyList.take();
//    }

    private static InputStream open(URL url, Proxy proxy) throws Exception {
        return url.openConnection(proxy).getInputStream();
    }

    public static void run(File cacheDir, Proxy proxy, URL baseURL, String[] args) throws Exception {

        //HACK this should probably be specified in app.txt
        System.setProperty("log4j.configuration", "utils/resources/log4j.properties");

        URL appURL = new URL(baseURL + "/webstart/app.txt");
        File appFile = new File(cacheDir, "app.txt");

        java.util.List<String> lines;
        try {
            lines = readLines(open(appURL, proxy));
        } catch (final Throwable t) {
            t.printStackTrace();
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    String errorMessage = "Cannot connect to the Starling server\nPlease ensure it is running and try again";
                    String title = "Cannot connect to the Starling server";
                    JOptionPane.showMessageDialog(null, errorMessage, title, JOptionPane.ERROR_MESSAGE);
                }
            });
            return;
        }

        String[] firstLine = lines.get(0).split(" ");
        String mainClass = firstLine[0];
        String[] appTxtArgs = Arrays.copyOfRange(firstLine, 1, firstLine.length);
        int applicationArgsLength = appTxtArgs.length + args.length;
        String[] applicationArgs = new String[applicationArgsLength];
        for (int i = 0; i < applicationArgsLength; i++) {
            if (i < appTxtArgs.length) {
                applicationArgs[i] = appTxtArgs[i];
            } else {
                applicationArgs[i] = args[i - appTxtArgs.length];
            }
        }
        Map<String, String> remoteJarsWithMd5 = jarsWithMD5(lines);
        Map<String, String> localJarsWithMD5 = new HashMap<String,String>();
        if (appFile.exists()) {
            localJarsWithMD5 = jarsWithMD5(readLines(new FileInputStream(appFile)));
        }

        Map<String, String> missingOrOutOfDateJars = new HashMap<String, String>();

        for (Map.Entry<String, String> entry : remoteJarsWithMd5.entrySet()) {
            String jar = entry.getKey();
            String md5 = entry.getValue();
            if (!localJarsWithMD5.containsKey(jar) || !md5.equals(localJarsWithMD5.get(jar))) {
                missingOrOutOfDateJars.put(jar, md5);
            }
        }

        for (Map.Entry<String, String> entry : missingOrOutOfDateJars.entrySet()) {
            String jar = entry.getKey();
            String md5 = entry.getValue();
            File file = new File(cacheDir, jar);
            URL url = new URL(baseURL + "/webstart/" + jar + "?md5=" + md5);
            System.out.println("downloading " + url);
            InputStream inputStream = open(url, proxy);
            OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(file));
            copy(inputStream, outputStream);
            inputStream.close();
            outputStream.flush();
            outputStream.close();
        }

        Set<String> unnecessaryJars = new HashSet<String>(localJarsWithMD5.keySet());
        unnecessaryJars.removeAll(remoteJarsWithMd5.keySet());
        for (String jar : unnecessaryJars) {
            new File(cacheDir, jar).delete();
        }
        List<URL> urlsToLocalJars = new LinkedList<URL>();
        for (String jar : remoteJarsWithMd5.keySet()) {
            urlsToLocalJars.add(new File(cacheDir, jar).toURI().toURL());
        }

        FileWriter app = new FileWriter(appFile);
        for (String line : lines) {
            app.append(line + "\n");
        }
        app.close();

        final URLClassLoader classLoader = new URLClassLoader(urlsToLocalJars.toArray(new URL[urlsToLocalJars.size()]));

        //all-permissions in the webstart jnlp file only applies to this class loader
        //the following code set the 'all' permission on the urlclassloader
        Policy.setPolicy(new Policy() {
            public PermissionCollection getPermissions(CodeSource codesource) {
                Permissions perms = new Permissions();
                perms.add(new AllPermission());
                return perms;
            }

            public void refresh() {
            }
        });

        System.out.println("Booter ready " + ManagementFactory.getRuntimeMXBean().getUptime() / 1000 + "s");
        System.out.flush();
        System.err.flush();
        try {
            Thread.currentThread().setContextClassLoader(classLoader);
            SwingUtilities.invokeAndWait(new Runnable() {
                public void run() {
                    UIManager.put("ClassLoader", classLoader); //for the look and feel
                    //trident uses the context classloader on the swing thread so we set it here
//          Thread.currentThread().setContextClassLoader(classLoader);
                }
            });
            Class launcher = classLoader.loadClass(mainClass);
            Method main = launcher.getMethod("main", new Class[]{String[].class});
            main.invoke(null, new Object[]{applicationArgs});
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static java.util.List<String> readLines(InputStream in) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(in));
        java.util.List<String> lines = new LinkedList<String>();
        {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line);
            }
        }
        return lines;
    }

    private static Map<String,String> jarsWithMD5(List<String> lines) {
        Map<String, String> jarsToMd5s = new HashMap<String, String>();
        for (String line : lines.subList(1, lines.size())) {
            String jar = line.split(" ")[0];
            String md5 = line.split(" ")[1];
            jarsToMd5s.put(jar, md5);
        }
        return jarsToMd5s;
    }

    private static long copy(InputStream input, OutputStream output) throws IOException {
        byte[] buffer = new byte[1024];
        long count = 0;
        int n;
        while (-1 != (n = input.read(buffer))) {
            output.write(buffer, 0, n);
            count += n;
        }
        return count;
    }
}

class TeeOutputStream extends OutputStream {

    private OutputStream a;
    private OutputStream b;

    public TeeOutputStream(OutputStream a, OutputStream b) {
        this.a = a;
        this.b = b;
    }

    public void write(int c) throws IOException {
        a.write(c);
        b.write(c);
    }

    public void flush() throws IOException {
        a.flush();
        b.flush();
    }
}