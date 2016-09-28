package input

import de.huberlin.informatik.promi.map.Prefix
import org.deckfour.xes.model.XTrace
import de.huberlin.informatik.promi.xes.XESImplicits._
import de.huberlin.informatik.promi.map.Output
import scala.collection.JavaConversions._

class ExamplePrefix extends Prefix {

  def getPrefix(trace: XTrace) = {
    trace.getPrefix(event =>
      event.getSimpleEventName() == "FP200 AntragBewilligen" ||
        event.getSimpleEventName() == "FP200 AntragAblehnen")
  } 
} 

class ExampleOutput1 extends Output {
  def getCategory(trace : XTrace) = 
    fromBoolean(trace.map { _.getSimpleEventName }.exists(_ == "FP200 AntragNeuBearbeiten"))
}

class ExampleOutput2 extends Output {
  def getCategory(trace : XTrace) = 
    fromBoolean(trace.map { _.getSimpleEventName }.exists(_ == "FP200 AntragWidersprechen"))
}
