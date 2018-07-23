package City

import scala.math
import java.io._

object main{
  def main(){
    val test = new City(1)
    val writer = new PrintWriter(new File("test1.csv" ))
    val resources_array = Array(//new Resource("food",1,10,10000,100,.5,0),
                          //new Resource("housing",1,10,10000,100,.25,0),
                          new Resource("lux",1500,10,100,100,1,0))
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
  var gdp:Double = 10000
  var optimism = .5 //split up optimism and pessimism?

  var savings_total:Double = 25000
  var savings_rate = .5

  def next_fiscal_quarter(resources: Array[Resource]){
    val resource_num = resources.length
    nextUnit(resources, gdp, resource_num)
  }

  def nextUnit(resources: Array[Resource], lastGDP: Double, resource_num:Int): Int={
    population = population * (1+fertility_rate-death_rate)
    val prices = Array.fill(resource_num)(0.0)
    val diff = Array.fill(resource_num)(0.0)
    var count = 0
    var change = lastGDP
    var gdp_c = 0.0
    var amount = 0.0
    var ready = 0.0
    var m_price = 0.0
    for(i <- resources){
      amount = i.desired
      ready= i.suppliers * i.average_supplied
      //why is m_price failing?
      m_price = scala.math.pow(i.unit_price,scala.math.pow((amount/ready),i.curve))
      diff(count) = ready - amount
      prices(count) = m_price
      change = change - m_price*amount
      count = count + 1
    }
    //When change is greater than 0, that means that demand is satisfied with less than it cost last month.
    //So I can anticipate spending more next month.
    //When change is less than 0, that means that demand was not satisfied with what I had last month, so I can't afford as much.
    //I take out the difference from my bank account and bake that into my estimates for next month.
    //If i don't have anything in my bank account, I need to cut back on my spending by that amount.
    count = 0
    if(change < 0){
      savings_total = savings_total - (-1 * change)
      if(savings_total < 0){
        gdp_c = gdp_c + savings_total
        for(i<-resources){
          gdp_c = gdp_c + i.desired * prices(count)
          i.desired = i.desired - savings_total/lastGDP * i.desired * (1 - optimism)
          count=count+1
        }
        savings_total = 0.0
      }
      else{
        for(i<-resources){
          gdp_c = gdp_c + i.desired * prices(count)
          i.desired = i.desired - change/lastGDP * i.desired * (1 - optimism) //should be closer to 0 than above
          count=count+1
        }
      }
    }
    else if(change > 0){
      savings_total = savings_total + change * savings_rate
      for(i<-resources){
        gdp_c = gdp_c + i.desired * prices(count)
        i.desired = i.desired + change*(1-savings_rate)/lastGDP * i.desired * (1 + optimism)
        count=count+1
      }
    }
    //Change how resources are produced based on demand.
    //Will change more as events occur that drive up or lower demand or supply
    count= 0
    for(i <- resources){
      if(math.abs(diff(count)) > i.average_supplied && diff(count)>0)
      {
        i.suppliers = i.suppliers - 1;
      }
      else if(math.abs(diff(count)) < i.average_supplied && diff(count) > 0)
      {
        i.average_supplied= i.average_supplied- (diff(count)/i.suppliers)
      }
      else if(math.abs(diff(count)) < i.average_supplied && diff(count) < 0)
      {
        i.average_supplied= i.average_supplied+ (diff(count)/i.suppliers)
      }
      else if(math.abs(diff(count)) > i.average_supplied && diff(count) < 0)
      {
        i.suppliers = i.suppliers + 1
      }
      count = count + 1
    }
    print(resources(0).toString())
    gdp = gdp_c
    return 1
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
