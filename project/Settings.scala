import sbt._

/**
  * Application settings. Configure the build for your application here.
  * You normally don't have to touch the actual build definition after this.
  */
object Settings {
    /** The name of your application */
    val name = "wtf"

    /** The version of your application */
    val version = "1.0"

    /** Options for the scala compiler */
    val scalacOptions = Seq(
        "-Xlint",
        "-unchecked",
        "-deprecation",
        "-feature"
    )

    /** Declare global dependency versions here to avoid mismatches in multi part dependencies */
    object versions {
        val scala = "2.12.1"
//        val scalaDom = "0.8.2"
//        val scalajsReact = "0.10.2"
//        val scalaCSS = "0.3.1"
//        val log4js = "1.4.10"
//        val autowire = "0.2.5"
//        val booPickle = "1.1.0"
//        val diode = "0.1.0"
//        val uTest = "0.3.1"

//        val react = "0.14.3"
//        val jQuery = "1.11.1"
//        val bootstrap = "3.3.2"
//        val chartjs = "1.0.1"
//
//        val playScripts = "0.3.0"
    }
}
