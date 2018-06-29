package City

import scala.math
import java.io._

object main{
  def main(){
    val test = new City(1)
    val writer = new PrintWriter(new File("test1.csv" ))
    for(i <- 0 to 20){
      test.next_fiscal_quarter()
      display.display_data(test, i, writer)
    }
    writer.close()
  }

}

object display{
  def display_data(city: City, count: Int, writer: PrintWriter){
    count.toString()
    val data = count.toString()+","+city.population.toString()+","+city.gdp.toString()+","+city.savings_average.toString()+","+city.prev_econ.toString()+"\n"
    print(data + "\n")
    writer.write(data)
  }
}

class City(var locl:Int){
  var population:Double = 100000
  var fertility_rate = .15
  var death_rate = .10
  var location = locl
  var gdp:Double = 1000000
  var optimism = 1.5

  var savings_total:Double = 25000
  var savings_rate = .5

  var investment_productive = .25

  val resources = Array(//new Resource("food",1,10,10000,100,.5,0,2),
                        //new Resource("housing",1,10,10000,100,.25,0,2),
                        new Resource("lux",1,10,10000,100,.05,0,4))
  val resource_num = resources.length

  var prev_econ=0

  def next_fiscal_quarter(){
    prev_econ = calculate_prices(resources)
  }

  def calculate_prices(resources: Array[Resource]): Int={
    print(resources(0).toString())
    val prices_init = Array.fill(resource_num)(0.0)
    val diff = Array.fill(resource_num)(0.0)
    val percentage_change = Array.fill(resource_num)(0.0)
    var count = 0
    for(i <- resources){
      val amount = i.desired
      val ready= i.suppliers * i.average_supplied + i.trade
      val difference = ready-amount
      val chunk = difference/i.average_supplied
      var price:Double = 0.0

      print("Difference "+ difference+"\n")

      if(ready > amount){
        price = (i.unit_price - (chunk * i.curve))
      }
      else{
        price = (i.unit_price + (chunk * i.curve))
      }
      prices_init(count) = price * i.desired
      diff(count) = difference
      count = count + 1
    }
    for(i<-resources){

      if(math.abs(diff(count)) > i.average_supplied && diff(count)>0)
      {
        i.suppliers = i.suppliers - 1;
        percentage_change(count)= i.suppliers/(i.suppliers+1)
        print("* "+1)
      }
      else if(math.abs(diff(count)) < i.average_supplied&& diff(count) > 0)
      {
        val cur = i.average_supplied
        i.average_supplied= i.average_supplied- (diff(count)/i.suppliers)
        percentage_change(count)= i.average_supplied / cur
        print("* "+2)
      }
      else if(math.abs(diff(count)) < i.average_supplied&& diff(count) < 0)
      {
        val cur = i.average_supplied
        i.average_supplied= i.average_supplied+ (diff(count)/i.suppliers)
        percentage_change(count)= i.average_supplied / cur
        print("* "+3)
      }
      else if(math.abs(diff(count)) > i.average_supplied&& diff(count) < 0)
      {
        i.suppliers = i.suppliers + 1
        percentage_change(count)= (i.suppliers/i.suppliers-1)
        print("* "+4)
      }
      count = count + 1
    }
    val ret = tick_economy(prices_init, percentage_change)

    count = 0

    print(prices_init(0)+ "\n")
    return ret
  }

  def tick_economy(prices: Array[Double], change: Array[Double]): Int ={
    var income_gap = gdp
    var total_exchange = 0.0
    var total_change = 0.0

    for(i <- 0 until prices.length){
      total_exchange = total_exchange + prices(i)
    }
    for(i <- 0 until prices.length){
      total_change=total_change+ change(i)*prices(i)/total_exchange
    }
    
    gdp = gdp * total_change
    population = population * (1 + fertility_rate-death_rate)

    if(income_gap>0){
      savings_average = savings_average + income_gap
      val growth_expected = (income_gap/income_average)*optimism
      for(i <- resources){
        i.desired = i.desired * growth_expected
      }
      return 1
    }
    else if(income_gap < 0){
      gdp = gdp - income_gap/savings_average
      savings_average = savings_average - income_gap/2
      val growth_expected = 1-(income_gap/income_average)
      for(i <- resources){
        i.desired = i.desired * growth_expected
      }
      return -1
    }
    else{
      return 0
    }
  }

}

class Resource(nam: String, desire: Double, supplier: Double, averag: Double, unit_pric: Float, curv: Double, trad: Double){
  val name = nam
  var desired:Double = desire
  var suppliers:Double = supplier
  var average_supplied:Double = averag
  var unit_price:Float = unit_pric
  var curve:Double = curv
  var trade = trad

  override def toString():String={
    return(nam+" \nDesired Amount "+desired.toString()+" \nSupplier Number "+suppliers.toString()+" \nAverage Supplied "+average_supplied.toString()+" \nUnit price "+unit_price.toString()+" \ncurve "+curve.toString()+"\n")

  }
}
