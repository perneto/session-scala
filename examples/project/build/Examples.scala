import sbt._

class Examples(info: ProjectInfo) extends DefaultProject(info) {
  override def fork = forkRun
}  
