import scala.math
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
    val data = count.toString()+",\t"+city.population.toString()+"\n"+city.toString()+"\n************\n"
    print(data)
    //print(city.toString())
    writer.write(data)
  }
}
class City(var locl:Int){
  var population:Double = 2000.0
  var fertility_rate = .15
  var death_rate = .10
  var location = locl
  var unemployed = 0.0
                      //class Industry(nam: String, input: Array[Double], output: Array[Double], worker: Double, in_storage: Double, pric: Double){
  val resources_array = Array(new Industry("food", Array(1.0,1.0,0.0), Array(3.0,0.0,0.0), 700.0, 600.0, 1.0),
                              new Industry("wood", Array(1.0,0.0,1.0), Array(0.0,3.0,0.0), 700.0, 600.0, 1.0),
                              new Industry("stone", Array(1.0,1.0,0.0), Array(0.0,0.0,3.0), 600.0, 600.0, 1.0))
  var excess_demand = false
  var excess_resourc = 0
  var excess_resourc_amnt = 0.0

  override def toString():String={
    return resources_array(0).toString()+resources_array(1).toString()+resources_array(2).toString()
  }

  def next_fiscal_quarter():Int={
    /*
   if enough storage -> storage += output * population
   Else, find the limiter
     limiter is defined as the resource with the greatest difference between currently stored and demanded
     evenly define based on number of workers
     recalculate based on utilization (remove that number of workers and add back to labor pool ?)
     if storage is still under demand,
       repeat?
    */
    unemployed = unemployed + population * (fertility_rate - death_rate)
    population = population + population * (fertility_rate - death_rate)
    findExcess()
    val output = Array.fill[Double](3)(0)
    while(excess_demand){
      for(i<-resources_array){
          if(i.inputs(excess_resourc)>0){
            //How much it needs to be decreased by (so the excess over the total),
            val diff = i.workers
            i.workers = (1 - excess_resourc_amnt/i.stored) * i.workers
            //print("Excess "+excess_resourc_amnt.toString() + " " + i.stored.toString()+ " " + (excess_resourc_amnt/i.stored).toString()+"\n")
            unemployed = unemployed + diff - i.workers
          }

       }
       findExcess()
    }

    for (i <- resources_array){
      for( j <- 0 to 2){
        print(i.name+" "+i.workers+"\n")
        resources_array(j).stored = resources_array(j).stored - i.inputs(j) * i.workers
        output(j) = output(j) + i.outputs(j) * i.workers
      }
    }

    //Allocation of new workers
    val unemploy = unemployed
    for (i <- resources_array){
      print(i.name+" "+i.stored+"\n")
      print("unemployment =" + unemployed + "\n")
      if(i.stored<5){
        i.workers = i.workers + unemploy/resources_array.length
        unemployed = unemployed - unemploy/resources_array.length
      }

    }
    for(i<- resources_array){
      i.workers = i.workers + unemployed/resources_array.length
    }
    unemployed = 0

    for (i <- 0 to 2){
      //print(resources_array(i).stored.toString() + " Stored  Output " + output(i).toString()+"\n")
      resources_array(i).stored = resources_array(i).stored + output(i)
      resources_array(i).produced = output(i)
    }

    return 1
  }

  def findExcess(){
    val storage = Array(resources_array(0).stored, resources_array(1).stored, resources_array(2).stored)
    val workers = Array.fill[Double](3)(0)
    val demand = Array.fill[Double](3)(0)
    var excess = false
    var ret = 0
    var amnt = 0.0

    for (i <- resources_array){
      demand(0) = demand(0) + i.inputs(0)*i.workers
      demand(1) = demand(1) + i.inputs(1)*i.workers
      demand(2) = demand(2) + i.inputs(2)*i.workers
      //Should technically be not zero, here they're all 1 or 0 so its fine as a demo
      workers(0) = workers(0) + i.inputs(0)*i.workers
      workers(1) = workers(1) + i.inputs(1)*i.workers
      workers(2) = workers(2) + i.inputs(2)*i.workers
    }

    for(i <- 0 to 2){
      if(demand(i) - storage(i) >= 1){
        excess = true
        if((demand(i) - storage(i))/storage(i) > amnt){
          ret = i
          amnt = (demand(i) - storage(i))/storage(i)
        }
      }
    }
    excess_demand = excess
    excess_resourc = ret
    excess_resourc_amnt = amnt

  }

}
class Industry(nam: String, input: Array[Double], output: Array[Double], worker: Double, in_storage: Double, pric: Double){
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
  var produced = 0.0


  override def toString():String={
    return(name+" \nWorkers "+workers.toString()+" \nStored "+stored.toString()+" \nProduced "+produced.toString()+" \nPrice "+price.toString()+" \n")

  }
}
