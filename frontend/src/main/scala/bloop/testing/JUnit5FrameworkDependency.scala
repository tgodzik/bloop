package bloop.testing

import bloop.DependencyResolution
import bloop.io.AbsolutePath
import bloop.logging.Logger

object JUnit5FrameworkDependency {
  private val junit5DepPattern = raw"junit-jupiter-.*\.jar".r
  private val jupiterInterfacePattern = raw"jupiter-interface-.*\.jar".r

  /**
   * Checks if JUnit 5 (Jupiter) dependencies are present in the classpath and automatically
   * adds the jupiter-interface dependency if it's missing. This is necessary because JUnit 5
   * doesn't implement the sbt test interface directly - it requires an adapter.
   *
   * @param rawClasspath The current classpath
   * @param logger Logger for diagnostic messages
   * @return The classpath, potentially with jupiter-interface added
   */
  def maybeAddJUnit5FrameworkDependency(
      rawClasspath: List[AbsolutePath],
      logger: Logger
  ): List[AbsolutePath] = {
    var junit5Dependency = false
    var jupiterInterfaceDependency = false

    rawClasspath.foreach { jar =>
      if (jar.isFile) {
        jar.underlying.getFileName().toString() match {
          case junit5DepPattern() =>
            junit5Dependency = true
          case jupiterInterfacePattern() =>
            jupiterInterfaceDependency = true
          case _ =>
        }
      }
    }

    val resolved =
      if (junit5Dependency && !jupiterInterfaceDependency) getJUnit5FrameworkDependency(logger)
      else None

    resolved.map(rawClasspath ++ _).getOrElse(rawClasspath)
  }

  private def getJUnit5FrameworkDependency(logger: Logger): Option[Array[AbsolutePath]] = {
    val jupiterInterface =
      DependencyResolution.Artifact("com.github.sbt.junit", "jupiter-interface", "0.17.0")
    DependencyResolution.resolveWithErrors(List(jupiterInterface), logger) match {
      case Left(error) =>
        logger.warn(s"Failed to resolve JUnit 5 jupiter-interface dependency: $error")
        None
      case Right(value) => Some(value)
    }
  }
}
