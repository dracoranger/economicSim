import java.io._

val RESOURCE_NUM = 3

object main{
  def main(){
    val test = new City(2000,.15,.10)
    val writer = new PrintWriter(new File("test1.csv" ))
    //class Industry(nam: String, input: Double, output: Double, worker: Int, in_storage: Double, price: Double){
    for(i <- 0 to 100){
      test.next_fiscal_quarter()
      display_data(test, i, writer)
    }
    writer.close()
  }
  def display_data(city: City, count: Int, writer: PrintWriter){
    val data = count.toString()+",\t"+city.population.toString()+"\n"+"name\t workers\t      stored\t\t produced\t price\n"+city.toString()
    print(data)
    print("\n************\n")
    //print(city.toString())
    writer.write(data)
  }
}

class World(var seed:Int, var size:Int){
  val grid = generate_grid(seed, size)
  val changed = 0 //hashtable of altered locations
  var city_order = 0 //priority queue of cities to be affected?
                    //Subdivide based on how nations? How long does it take to move between states?  Is that relevant?
  var nations = 0 // array of nations

  def dijkstra_generator(){
    //Check number of trade routes, generate list for that number, give in closeness order
    for(i<- 0 until size){
      for(j<- 0 until size){
        val trade_limit = grid[i][j].trade_route_number
        var curr_nearest_neighbors = 0

        val active = mutable.Set(source)
        val res = mutable.Map(source -> 0)
        var costs = mutable.Map.empty[N, N]//WHAT IS N?

        while(curr_nearest_neighbors < trade_limit){ //Need to finish
          val node = active.minBy(res)
          active -= node
          val cost = res(node)
          for ((n, c) <- g(node)) {
            val cost1 = cost + c
            if (cost1 < res.getOrElse(n, Int.MaxValue)) {
              active += n
              res += (n -> cost1)
              pred += (n -> node)
            }

          }

        }
        grid[i][j].update_neighbors(res)
        grid[i][j].update_tree(costs)
      }

    }

  }

  def next_fiscal_quarter(){
    //reassign any square that has been altered
    var changedSquares = scala.mutable.Map
    for(i<-nations){
      changedSquares = changedSquares + i.city_capture()
    }
    for(i<-changedSquares){//What happens when captured and recaptured?
      grid(i._1)(i._2).owner = i
      i.usedToOwn(city_loss(i._1,i._2))
    }

    //Spin off for each nation
    for(i<-nations){
      i.next_fiscal_quarter()
    }

    //Global trade?

  }

  def calculateType(var i:Int, var j:Int) = {
    Square(Double(i)+1,Double(j)+1,new City(1000,.15,.10),10)
  }

  def generate_grid(var seed, var size: Int):Array[Square][Square]={
    //Generate randomized world
    //For now, just build identical location
    val ret = Array.tabulate(size,size)(calculateType)
    ret
  }


}

class Nation(var global_trade:Int){
  val national_stores = Array.fill[Double](RESOURCE_NUM)(0)
  val civil_servants = 0 //Number of population working for the government, rather than in production
  val cities = 0 //priority queue of cities based on population
                 //also contains all the squares
                 //make it so that
  var willingness_to_trade = global_trade //beginning of differentiating AI
  val squares = scala.mutable.Map() //(x,y) -> Square
  //Move djistra's here and recalculate when gained?

  def next_fiscal_quarter(){//Just do it in order
    //recalculate based on size?
    //Need to relearn priority queues
    
    for(i<-squares){
      i.next_fiscal_quarter()
    }
  }

  def city_capture(){

  }
  def city_loss(){

  }
  def trade_request(){

  }
  def trade_give(){

  }

}

class Square(var trav: Double, var typeOfLand:Int, var cit: City, var trade_route_num:Int){
  var square_storage = Array.fill[Double](RESOURCE_NUM)(1000)
  var trade_desired = Array.fill[Double](RESOURCE_NUM)(0)
  var warehouse = Array.fill[Double](RESOURCE_NUM)(0)
  var traversal_cost = trav
  var city = cit
  var owner = 0
  var sema = True
  val neighbor_queue = 0
  var trade_route_number = trade_route_num
  var trade_routes = 0 //Need to implement paths to allow for updating trade relationships.



  def next_fiscal_quarter(){
    trade_desired = city.request_trade()
    for(i <- neighbor_queue){
      get_trade(i, trade_desired(), warehouse)
    }
    city.recieve_trade(warehouse)
    city.next_fiscal_quarter()
    square_storage = city.reserve_next_quantity(square_storage)
  }

  def get_trade(var neighbor:Square, var trade_requested:Array[Double], var gotten:Array[Double]){
    for(i<-0 until RESOURCE_NUM){
      gotten(i) = neighbor.give_trade(trade_requested(i),i)
    }
  }

  def give_trade(var requested: Double, var resource: Int){
    if (trade(resource) >= requested){
      trade(resource) = trade(resource) - requested
      return requested
    }
    else{
      var ret = 0
      ret = trade(resource)
      trade(resource) = 0
      return ret
    }
  }

  def update_neighbors(var altered:Square){
    //find the square in the trade_routes
    //update all in chain based off that.
  }

}

class City(var pop:Double, var fert:Double, var dea:Double){
  var population:Double = pop
  var fertility_rate = fert
  var death_rate = dea
  var unemployed = 0.0
                      //class Industry(nam: String, input: Array[Double], output: Array[Double], worker: Double, in_storage: Double, pric: Double){
  val resources_array = Array(new Industry("food", Array(0.0,1.0,0.0), Array(2.0,0.0,0.0), 1000.0, 600.0, 1.0),//Bug if good needs itself for production
                              new Industry("wood", Array(1.0,0.0,1.0), Array(0.0,3.0,0.0), 999.0, 600.0, 1.0),
                              new Industry("stone", Array(1.0,1.0,0.0), Array(0.0,0.0,3.0), 1.0, 600.0, 1.0))
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
    val output = Array.fill[Double](RESOURCE_NUM)(0)
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
        resources_array(j).stored = resources_array(j).stored - i.inputs(j) * i.workers
        output(j) = output(j) + i.outputs(j) * i.workers
      }
    }

    //Allocation of new workers
    val unemploy = unemployed
    for (i <- resources_array){
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

  def request_trade():Array[Double]={
    val storage = Array(resources_array(0).stored, resources_array(1).stored, resources_array(2).stored)
    val workers = Array.fill[Double](RESOURCE_NUM)(0)
    val demand = Array.fill[Double](RESOURCE_NUM)(0)
    val shortage = storage
    var excess = false
    var ret = 0
    var amnt = 0.0

    for (i <- resources_array){
      for (j <- 0 until RESOURCE_NUM){
        shortage(j) = shortage(j) - i.inputs(j)
      }
    }
    return shortage
  }

  def recieve_trade(var materials: Array[Double]){
    for(i <- 0 until RESOURCE_NUM){
      resources_array(i).storage = resources_array(i).storage + materials(i)
    }

  }

  def find_excess(){
    val storage = Array(resources_array(0).stored, resources_array(1).stored, resources_array(2).stored)
    val workers = Array.fill[Double](RESOURCE_NUM)(0)
    val demand = Array.fill[Double](RESOURCE_NUM)(0)
    var excess = false
    var ret = 0
    var amnt = 0.0

    for (i <- resources_array){
      for (j <- 0 until RESOURCE_NUM){
        demand(j) = demand(j) + i.inputs(j)*i.workers
        if (i.inputs(j) > 0){
          workers(j) = workers(j) + i.workers
        }
      }
    }

    for(i <- 0 to 2){
      if(demand(i) - storage(i) >= demand(i)/workers(i)){ //Now if demand difference is less than 1 worker, we can just ignore, rather than arbitrary value
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

  def reserve_next_quantity(var squareReserves:Array[Double]):Array[Double]={
    var ret = Array.fill[Double](RESOURCE_NUM)(0)
    val demand = Array.fill[Double](RESOURCE_NUM)(0)

    for (i <- resources_array){
      for (j <- 0 until RESOURCE_NUM){
        demand(j) = demand(j) + i.inputs(j)*i.workers
      }
    }
    for(i <- 0 until RESOURCE_NUM){
      if squareReserves(i)>demand(i){
        ret(i) = squareReserves(i) - demand(i)
        resources_array(i).storage = demand(i)
      }
      else{
        ret(i) = 0
        resources_array(i).storage = squareReserves(i)
      }
    }
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
    return(name+", "+workers.toString()+", "+stored.toString()+", "+produced.toString()+", "+price.toString()+" \n")

  }
}
