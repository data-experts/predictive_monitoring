package de.huberlin.informatik.promi.experiments

import java.io.FileInputStream
import java.io.Closeable
import scala.util.control.Exception._
import java.util.Properties
import de.huberlin.informatik.promi.map.Prefix
import de.huberlin.informatik.promi.map.Output
import de.huberlin.informatik.promi.util.Splitter
import de.huberlin.informatik.promi.experiments.outlier.Outlier
import de.huberlin.informatik.promi.experiments.outlier.OCSVMRBF
import de.huberlin.informatik.promi.util.Splitter.TrainingTest
import java.io.File

object RunScripted {

  def main(args: Array[String]): Unit = {
    implicit val loadedProps: java.util.Properties = new Properties()
    withCloseable(new FileInputStream("config.properties")) { fis =>
      loadedProps.load(fis)
    }
    if (isRandomForest(loadedProps)) {
      (new RunScripted with BinaryClassification.WithRandomForestClassifier).main(args)
    } 
    else if (isOutlierDetection(loadedProps)){
      (new RunScripted with OCSVMRBF).main(args)
    }
  }

  def withCloseable[T <: Closeable, R](t: T)(f: T => R): R = {
    allCatch.andFinally { t.close } apply { f(t) }
  }
  
  def isRandomForest( props : Properties ) : Boolean = getProp("type")(props).equals("randomforest")
  def isOutlierDetection( props : Properties ) : Boolean = getProp("type")(props).equals("outlier")

  def getProp( name : String, mandatory : Boolean = true)(implicit props : Properties) = {
    val prop = props.getProperty(name)
    if (prop == null && mandatory) throw new IllegalArgumentException(s"entry '$name' is mandatory in configuration")
    else if( prop == null) prop
    else prop.toString
  }
  
}

abstract class RunScripted(implicit val props : Properties) {
  
  def files(folder: String) : Seq[String] = {
    val f = new File(folder)
    if (!f.exists()) {
      throw new IllegalArgumentException(f.getPath + " does not exist")
    }
    f.list().toSeq
  }
  
  import RunScripted._
  val trainFolder = getProp("training")
  val testFolder = getProp("test")
  def prefix: Prefix = Class.forName(getProp("prefix")).newInstance.asInstanceOf[Prefix]
  def output: Output = Class.forName(getProp("output")).newInstance.asInstanceOf[Output]
  def attributes: Seq[String] = "Label" +: Option(getProp("ignore-attributes", false)).map { _.split(',').toSeq }.getOrElse(Seq())
  def title : String = getProp("title")
  def noiseTreeMiner : Option[Float]= Option(getProp("IM-noise", false)).map(_.toFloat)
  def split : Splitter.Split = TrainingTest(trainFolder, testFolder)
  def fileNames : Seq[String] = files(trainFolder) ++ files(testFolder)
  def abbrv : String = ""
  def nu = Option(getProp("nu", false)).map(_.toDouble).getOrElse(0.05)
}