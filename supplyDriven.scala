import scala.math
import java.io._

object main{
  def main(){
    val test = new City(1)
    val writer = new PrintWriter(new File("test1.csv" ))
    //class Industry(nam: String, input: Double, output: Double, worker: Int, in_storage: Double, price: Double){
    val resources_array = Array(new Industry("food",1,10,10000,100,.5,0),
                          new Industry("wood",1,10,10000,100,.25,0),
                          new Industry("stone",900,10,100,10,.5))
    for(i <- 0 to 10){
      test.next_fiscal_quarter(resources_array)
      display.display_data(test, i, writer)
    }
    writer.close()
  }
  def display_data(city: City, count: Int, writer: PrintWriter){
    count.toString()
    val data = count.toString()+",\t"+city.population.toString()+",\t"+city.gdp.toString()+",\t"+city.savings_total.toString()+"\n"
    print("Count\tPopulation\t\tGDP\t\tsavings\n")
    print(data + "\n")
    writer.write(data)
  }

class City(var locl:Int){
  var population:Double = 2000
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
class Industry(nam: String, input: Double, output: Double, worker: Int, in_storage: Double, price: Double){
  val name = nam
  val inputs = input
  val outputs = output
  var productivity_growth = 0
  var resource_efficency_growth = 0
  var workers = worker
  var in_storage = stored
  var last_produced = 500
  var last_used = 500
  var price = pric
  var produced = 0

  def tickFinish(){


  }
  def tickStart(){


  }
  def findBottleneck(){

  }

  override def toString():String={
    return(name+" \nWorkers "+workers.toString()+" \nStored "+in_storage.toString()+" \nProduced "+produced.toString()+" \nPrice "+price.toString()+" \n")

  }
}
