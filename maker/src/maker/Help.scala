package maker

object Help{
  val text = """
  |
  |Maker has four main concepts. 
  |
  | a) Modules  :- These correspond pretty much exactly to modules in IntelliJ or Eclipse
  |                Modules have a dependency on upstream modules and also external libraries.
  |                Unlike IntelliJ, external library dependencies are automatically exported to
  |                downstream modules.
  |
  | b) Projects :- A collection of modules - just like an IntelliJ project. 
  |
  | c) Tasks    :- An atomic operations - normally performed in the context of a particular module
  |                or Project.
  |
  | d) Builds   :- A directed graph of tasks. Most user interaction with Maker is to cause it to
  |                execute Builds.
  | 
  |Enter <module>.builds or <project>.builds to see a list of available builds and how to execute them
  """.stripMargin


  def help{
    println("Helping")
    println(text)
  }
  def help(thing : String){
    println(thing)
  }
}


