import scala.math
import scala.collection.mutable.Stack
import java.io._


object main{
  def main(){
    val test = new City(1)
    val writer = new PrintWriter(new File("test1.csv" ))
    //class Industry(nam: String, input: Double, output: Double, worker: Int, in_storage: Double, price: Double){
    for(i <- 0 to 10){
      test.next_fiscal_quarter()
      display_data(test, i, writer)
    }
    writer.close()
  }
  def display_data(city: City, count: Int, writer: PrintWriter){
    count.toString()
    val data = count.toString()+",\t"+city.population.toString()+"\n"
    print("Count\tPopulation\t\tGDP\t\tsavings\n")
    print(data + "\n")
    writer.write(data)
  }
}
class City(var locl:Int){
  var population:Double = 2000
  var fertility_rate = .15
  var death_rate = .10
  var location = locl
                      //nam: String, input: Array[Double], output: Array[Double], worker: Int, in_storage: Double, pric: Double)
  val resources_array = Array(new Industry("food", Array(1.0,1.0,0.0), Array(3.0,0.0,0.0), 700, 600.0, 1.0),
                              new Industry("wood", Array(1.0,0.0,1.0), Array(0.0,3.0,0.0), 700, 600.0, 1.0),
                              new Industry("stone" Array(1.0,1.0,0.0), Array(0.0,0.0,3.0), 600, 600.0, 1.0))
  def next_fiscal_quarter():Int={
    val storage = Array(resources_array(0).stored,resources_array(1).stored,resources_array(2).stored)
    val demand = Array.fill[Double](3)(0)
    val total_workers = 0
    var excess_demand = Stack[Boolean]()
    var excess_demand_vals = Stack[Int]()
    var how_much = Stack[Double]()
    for (i <- resources_array){
      demand(0) = demand(0) + i.inputs(0)*i.workers
      demand(1) = demand(1) + i.inputs(1)*i.workers
      demand(2) = demand(2) + i.inputs(2)*i.workers
      total_workers = total_workers + i.workers
    }

    for(i <- 0 to 3){
      if(demand(i)>storage(i)){
        excess_demand.push(true)
        excess_demand_vals.push(i)
      }
      else{
        excess_demand.push(false)
      }
      how_much.push(demand(i)-storage(i))
    }

    while(!excess_demand.isEmpty){
      if(excess_demand.pop){
        var resource_demanded = excess_demand_vals.pop
        for(i <- resources_array){
          if(i.input(resource_demanded)>0){

          }
        }
      }
      else{

      }
    }
    return 1
  }

  def tickFinish(){


  }
  def tickStart(){


  }
  def findBottleneck(){

  }


}
class Industry(nam: String, input: Array[Double], output: Array[Double], worker: Int, in_storage: Double, pric: Double){
  val name = nam
  val inputs = input
  val outputs = output
  var productivity_growth = 0
  var resource_efficency_growth = 0
  var workers = worker
  var stored = in_storage
  var last_produced = 500
  var last_used = 500
  var price = pric
  var produced = 0


  override def toString():String={
    return(name+" \nWorkers "+workers.toString()+" \nStored "+in_storage.toString()+" \nProduced "+produced.toString()+" \nPrice "+price.toString()+" \n")

  }
}
