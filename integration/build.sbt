name := "bippy"

organization := "aftok"

version := "0.0.1"

libraryDependencies ++= Seq(
	"org.bitcoinj" % "bitcoinj-core" % "0.14.2",
	"org.specs2" %% "specs2-core" % "3.8.3" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
