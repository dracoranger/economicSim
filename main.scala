
import City.City
import java.io._
class main(){
  def main(){
    val test = new City(1)
    for(i <- 0 to 100){
      test.tick_economy()
      display_data(test, i)
    }
  }

  def display_data(city: City, count: Int){
    val writer = new PrintWriter(new File("test1.csv" ))
    count.toString()
    val data = count.toString()+","+city.population.toString()+","+city.gdp.toString()+","+city.savings_average.toString()+","+city.resources(0).desired.toString()+","+city.resources(1).desired.toString()+","+city.resources(3).desired.toString()+"\n"
    writer.write(data)
    writer.close()
  }

}
/*
var population:Double = 1000
var gdp:Double = 1000000

var savings_average:Double = 250
var savings_rate = .5

var investment_productive = .25

val resources = Array(new Resource("food",100,10,10000,1,.95,0,2),
                      new Resource("housing",100,10,10000,1,.75,0,2),
                      new Resource("lux",100,10,10000,1,.50,0,4))

  val name = nam
  var desired:Double = desire
  var suppliers:Double = supplier
  var average:Double = averag
  var unit_price:Float = unit_pric
  var curve:Double = curv
  var trade = trad
  var min = mi
*/
