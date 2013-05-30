import com.typesafe.zinc.Compiler
import maker.task.compile._

// stolen from Josh
object StronglyConnected {
  def apply(nodes: Set[File], dependencies: File => Set[File]): Set[Set[File]] = {
    val stack = new collection.mutable.Stack[File]
    val onStack = new collection.mutable.HashSet[File]
    val scc = new collection.mutable.ArrayBuffer[Set[File]]
    val index = new collection.mutable.ArrayBuffer[File]
    val lowLink = new collection.mutable.HashMap[File, Int]

    def tarjanImpl(v: File)
    {
      index += v
      lowLink(v) = index.size-1
      stack.push(v)
      onStack += v
      for(n <- dependencies(v))
      {
        if( !index.contains(n) )
        {
          tarjanImpl(n)
          lowLink(v) = math.min(lowLink(v), lowLink(n))
        }
        else if(onStack(n))
          lowLink(v) = math.min(lowLink(v), index.indexOf(n))
      }

      if(lowLink(v) == index.indexOf(v))
      {
        val components = new collection.mutable.ArrayBuffer[File]
        def popLoop()
        {
          val popped = stack.pop()
          onStack -= popped
          components.append(popped)
          if(popped != v) popLoop()
        }
        popLoop()
        scc.append(components.toSet)
      }
    }
    for(node <- nodes)
        if( !index.contains(node) )
          tarjanImpl(node)
    scc.toSet
  
  }
}

class Analysis(phase : ProjectPhase){
  val sourceFiles = phase.upstreamProjectPhases.flatMap(_.sourceFiles)
  val classFiles = phase.upstreamProjectPhases.flatMap(_.classFiles)
  val classNames = phase.upstreamProjectPhases.flatMap(_.classNames)
  val names : Map[File, String] = (classFiles ++ sourceFiles).map{
    f ⇒ (f → fileToName(f))
  }.toMap.filter{
    case(f, maybeName) ⇒ maybeName.isDefined
  }.mapValuesEagerly(_.get)

  def sfile(name : String) : File = sourceFiles.find(_.getName==name + ".scala").get
  def cfile(name : String) : File = classFiles.find(_.getName==name + ".class").get
  def cname(name : String) : String = classNames.find(_.endsWith("." + name)).get
  val a = phase.upstreamProjectPhases.map{pp ⇒ Compiler.analysis(pp.compilationCacheFile)}.reduce(_++_)
  def fileToName(f : File) : Option[String] = {
    val path = f.getPath
    val starlingIndex = path.lastIndexOf("starling")
    if (starlingIndex < 0)
      None
    else {
      Some(path.substring(starlingIndex).dropRight(6).replace('/', '.'))
    }
  }

  lazy val usesInternalSrcMap : Map[File, Set[File]] = sourceFiles.map{sf ⇒ (sf → a.relations.usesInternalSrc(sf))}.toMap.filter{case (_, fs) ⇒ fs.nonEmpty}
  lazy val usesExternalMap : Map[String, Set[File]] = classNames.map{name ⇒ (name → a.relations.usesExternal(name))}.toMap.filter{case (_, fs) ⇒ fs.nonEmpty}

  def immediateDownstreamF(file : File) : Set[File] = {
    
    val externalDownstream = names.get(file) match {
      case Some(name) ⇒ usesExternalMap.getOrElse(name, Set[File]())
      case None ⇒ Set.empty
    }
    val internalDownstream = usesInternalSrcMap.getOrElse(file, Set[File]())
    externalDownstream ++ internalDownstream
  }

  def immediateDownstream(name : String) = immediateDownstreamF(sfile(name)).map(_.getName)

  def transitiveDownstreamF(
    file : File
  ) = {
    var cnt = 0
    def rec(files : Set[File]) : Set[File] = {
      cnt += 1
      val files2 = files.flatMap(immediateDownstreamF)
      if (files == files ++ files2)
        files
      else
        rec(files ++ files2)
    }
    rec(Set(file))
  }

  def transitiveDownstream(name : String) : Set[String] = transitiveDownstreamF(sfile(name)).map(_.getName)

  def connectF(from : File, to : File) : List[Set[File]] = {
    def rec(acc : List[Set[File]]) : List[Set[File]] = {
      if (acc.head.contains(to))
        acc.reverse
      else {
        val alreadySeen : Set[File] = acc.flatten.toSet
        val next = {
          val cands = acc.head.flatMap(immediateDownstreamF).filterNot(alreadySeen.contains)
          cands.filter{
            c ⇒ 
            transitiveDownstreamF(c).contains(to)
          }
        }
        if (next.isEmpty)
          Nil
        else
          rec(next :: acc)
      }
    }
    rec(List(Set(from)))
  }
  def connect(from : String, to : String) : List[Set[String]] = {
    connectF(sfile(from), sfile(to)).map(_.map(_.getName))
  }
  def nonTrivial(initial : File) = {
    val dependsOnSrc : File ⇒ Set[File] = a.relations.usesInternalSrc _
    val initAndImmediate : Set[File] = Set(initial) ++ Set(initial).flatMap(dependsOnSrc)
    val components = StronglyConnected(initAndImmediate, dependsOnSrc)
    components
  }
  
}

