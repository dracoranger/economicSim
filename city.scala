package City

import scala.math
import java.io._

object main{
  def main(){
    val test = new City(1)
    val writer = new PrintWriter(new File("test1.csv" ))
    val data = test.population.toString()+","+test.gdp.toString()+","+test.savings_average.toString()+","+test.prev_econ.toString()+"\n"
    print(data)
    print("\n")
    for(i <- 0 to 5){
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
    print("\n")
    print(data)
    print("\n")
    writer.write(data)

  }
}

class City(var locl:Int){
  var population:Double = 1000
  var fertility_rate = .15
  var death_rate = .10
  var location = locl
  var gdp:Double = 1000000

  var savings_average:Double = 250
  var savings_rate = .5

  var investment_productive = .25

  val resources = Array(//new Resource("food",1,10,10000,100,.5,0,2),
                        //new Resource("housing",1,10,10000,100,.25,0,2),
                        new Resource("lux",1,11,10000,100,.05,0,4))
  val resource_num = resources.length

  var prev_econ=0

  def next_fiscal_quarter(){
    prev_econ = calculate_prices(resources)
  }

  def calculate_prices(resources: Array[Resource]): Int={
    val prices_init = Array.fill(resource_num)(0.0)
    val diff = Array.fill(resource_num)(0.0)
    var count = 0
    for(i <- resources){
      val amount = i.desired * population
      val ready= i.suppliers * i.average + i.trade
      val difference = math.abs(ready-amount)
      val chunk = difference/100
      var price:Double = 0.0
      print("Prices calcs "+amount+" "+ready+" "+difference+" "+chunk+" \\ ")
      if(ready > amount){
        price = (i.unit_price - chunk * i.curve) * amount
      }
      else{
        price = (i.unit_price + chunk * i.curve) * amount
      }
      prices_init(count) = price
      diff(count) = difference
      count = count + 1
    }
    print(prices_init(0).toString()+" ")
    val ret = tick_economy(prices_init)

    count = 0
    for(i<-resources){
      if(diff(count) > i.average && diff(count)>0) i.suppliers = i.suppliers - 1
      else if(diff(count) < i.average && diff(count) > 0) i.average = i.average - diff(count)/i.suppliers
      else if(diff(count) < i.average && diff(count) < 0) i.average = i.average + diff(count)/i.suppliers
      else if(diff(count) > i.average) diff(count) = i.suppliers + 1
      count = count + 1
    }
    return ret
  }

  def tick_economy(prices: Array[Double]): Int ={
    //choose amount of a good if can't be satisfied based on above or below average
    val income_average = gdp/population
    var income_gap = income_average
    for(i <- 0 until prices.length){
      income_gap = income_gap - prices(i)

    }
    population = population * (1 + fertility_rate-death_rate)
    print(gdp+" "+income_gap+" * ")
    if(income_gap>0){
      savings_average = savings_average + savings_rate*income_gap
      gdp = gdp + (investment_productive*savings_rate*income_gap + (1 - savings_rate) * income_gap) * population
      val growth_expected = income_gap/income_average
      for(i <- resources){
        i.desired = i.desired * growth_expected
      }
      return 1
    }
    else if(income_gap < 0){
      savings_average = savings_average - income_gap
      if(savings_average <= 0){
        print(gdp+" "+income_gap+" * ")
        gdp = gdp - (income_gap*population)
        var crash:Double = 0
        var iter = 0
        for(i <- resources){
          crash = crash + prices(iter)*i.desired/i.min
          iter = iter + 1
        }
        //if(savings_average * -1 > crash){
        //  migration()
        //}
        savings_average = 0
      }
      print(gdp+" "+income_gap+" | ")
      gdp = gdp + (investment_productive*savings_rate*income_gap) * population

      val growth_expected = (income_gap/income_average)/2
      for(i <- resources){
        i.desired = i.desired * growth_expected
      }
      return -1
    }
    else{
      return 0
    }
  }



  def migration(){
    population = population * .8
    gdp = gdp * .9
    death_rate = death_rate + .1
    for(i <- resources){
      i.desired = i.desired * .75
    }
  }
}

class Resource(nam: String, desire: Double, supplier: Double, averag: Double, unit_pric: Float, curv: Double, trad: Double, mi: Double){
  val name = nam
  var desired:Double = desire
  var suppliers:Double = supplier
  var average:Double = averag
  var unit_price:Float = unit_pric
  var curve:Double = curv
  var trade = trad
  var min = mi

  override def toString():String={
    return(nam+" "+desired.toString()+" "+suppliers.toString()+" "+average.toString()+" "+unit_price.toString()+" "+curve.toString()+" "+min.toString())

  }
}

/*
var food_desired:Double = 100
var housing_desired:Double = 100
var misc_desired:Double = 100

var housing_suppliers:Double = 10
var food_suppliers:Double = 10
var misc_suppliers:Double = 10

var housing_average:Double = 10000
var food_average:Double = 10000
var misc_average:Double = 10000

var housing_unit_price:Float = 1
var food_unit_price:Float = 1
var misc_unit_price:Float = 1

var housing_curve:Double = 1.75
var food_curve:Double = 1.95
var misc_curve:Double = 1.5

var housing_trade = 0
var food_trade = 0
var misc_trade = 0

var lux_min = 4

def calculate_prices(){
  val housing_amount = housing_desired * population
  val food_amount = food_desired * population
  val misc_amount = misc_desired * population

  val housing_ready= housing_suppliers * housing_average + housing_trade
  val food_ready= food_suppliers * food_average + food_trade
  val misc_ready= misc_suppliers * misc_average + misc_trade

  val housing_difference = math.abs(housing_ready-housing_amount)
  val food_difference = food_ready-food_amount
  val misc_difference = misc_ready-misc_amount

  val housing_chunk = housing_difference/100
  val food_chunk = food_difference/100
  val misc_chunk = misc_difference/100
  //Curve and chunk interaction doesn't work
  val housing_price = housing_chunk * housing_curve * housing_unit_price * housing_amount
  val food_price = food_chunk * food_curve * food_unit_price * food_amount
  val misc_price = misc_chunk * misc_curve * misc_unit_price * misc_amount

  tick_economy(housing_price, food_price, misc_price)

  //alter number of businesses as a result absolute values?
  if(housing_difference > housing_average && housing_difference>0) housing_suppliers = housing_suppliers - 1
  else if(housing_difference < housing_average && housing_difference > 0) housing_average = housing_average - housing_difference/housing_suppliers
  else if(housing_difference < housing_average && housing_difference < 0) housing_average = housing_average - housing_difference/housing_suppliers
  else if(housing_difference > housing_average) housing_suppliers = housing_suppliers + 1
  //Generalize for any resource


}

def tick_economy(prices: Array[Double]): Int ={
  //choose amount of a good if can't be satisfied based on above or below average
  val income_average = gdp/population
  val income_gap = income_average
  for(i <- 0 to len(prices)):{
    income_gap = income_gap - prices(i) * resources(i)(1)
  }
  val income_gap = income_average - (food_price*food_desired + housing_price*housing_desired + misc_price*misc_desired)
  population = population * (1 + fertility_rate-death_rate)

  if(income_gap>0){
    savings_average = savings_average + savings_rate*income_gap
    gdp = gdp + (investment_productive*savings_rate*income_gap + (1 - savings_rate) * income_gap) * population

    val growth_expected = income_gap/income_average

    food_desired = food_desired * growth_expected
    housing_desired = housing_desired * growth_expected
    misc_desired = misc_desired * growth_expected
    return 1
  }
  else if(income_gap < 0){
    savings_average = savings_average - income_gap
    if(savings_average <= 0){
      gdp = gdp - (income_gap*population)
      if(savings_average * -1 > (food_price*(food_desired/2) + housing_price*(housing_desired/2) + misc_price*(misc_desired/lux_min))){
        migration()
      }
      savings_average = 0
    }

    gdp = gdp + (investment_productive*savings_rate*income_gap) * population

    val growth_expected = (income_gap/income_average)/2
    food_desired = food_desired * growth_expected
    housing_desired = housing_desired * growth_expected
    misc_desired = misc_desired * growth_expected
    return -1
  }
  else{
    return 0
  }

}
*/
