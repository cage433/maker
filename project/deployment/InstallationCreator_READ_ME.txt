The installer should be created on the fly on the server but I can't be bothered to do that right now.

So, to create an installer for any particular server go to the servers webstart page:

http://{server}:{port}/webstart

Download StarlingProductionInstallScript.nsi to a directory on your windows machine.

Download the Starling-{serverName}.exe, excel_plugin-{serverName}.ini and excel_plugin={serverName}.xll files to the
same directory.

Launch NSIS, click on Compile NSI scripts. Load the *.nsi file you've just downloaded and an installer should be created
in the same directory.

Congratulate yourself on a job well done.