package City

import scala.math
import java.io._

object main{
  def main(){
    val test = new City(1)
    val writer = new PrintWriter(new File("test1.csv" ))
    //Resource(nam: String, desire: Double, supplier: Double, averag: Double, unit_pric: Double, curv: Double)
    val resources_array = Array(//new Resource("food",1,10,10000,100,.5,0),
                          //new Resource("housing",1,10,10000,100,.25,0),
                          new Resource("lux",900,10,100,10,.5))
    for(i <- 0 to 10){
      test.next_fiscal_quarter(resources_array)
      display.display_data(test, i, writer)
    }
    writer.close()
  }

}

object display{
  def display_data(city: City, count: Int, writer: PrintWriter){
    count.toString()
    val data = count.toString()+",\t"+city.population.toString()+",\t"+city.gdp.toString()+",\t"+city.savings_total.toString()+"\n"
    print("Count\tPopulation\t\tGDP\t\tsavings\n")
    print(data + "\n")
    writer.write(data)
  }
}

class City(var locl:Int){
  var population:Double = 1000
  var fertility_rate = .15
  var death_rate = .10
  var location = locl
  var gdp:Double = 100
  var optimism = .5 //split up optimism and pessimism?

  var savings_total:Double = 200
  var savings_rate = .5

  def next_fiscal_quarter(resources: Array[Resource]){
    val resource_num = resources.length
    nextUnit(resources, gdp, resource_num)
  }

  def nextUnit(resources: Array[Resource], lastGDP: Double, resource_num:Int): Int={
    population = population * (1+fertility_rate-death_rate)
    for(i <- resources){
      var amount = i.desired
      val ready= i.suppliers * i.average_supplied
      val diff = amount/ready
      val price = scala.math.pow(i.unit_price,scala.math.pow(diff,i.curve))

      i.desired = (amount / diff) / price/i.unit_price
      i.average_supplied = (i.average_supplied * diff) * price/i.unit_price
      //i.suppliers = (i.suppliers * diff) * price/i.unit_price
    }
    print(resources(0).toString())
    return 1
  }

}
class Resource(nam: String, desire: Double, supplier: Double, averag: Double, unit_pric: Double, curv: Double){
  val name = nam
  var desired:Double = desire
  var suppliers:Double = supplier
  var average_supplied:Double = averag
  var unit_price:Double = unit_pric
  var curve:Double = curv

  override def toString():String={
    return(nam+" \nDesired Amount "+desired.toString()+" \nSupplier Number "+suppliers.toString()+" \nAverage Supplied "+average_supplied.toString()+" \nUnit price "+unit_price.toString()+" \ncurve "+curve.toString()+"\n")

  }
}
