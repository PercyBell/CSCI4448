
// Percy Bell : CSCI 4448 : March 1 2019
// HW#3 Code

import scala.util.Random


// Must know its type, its ID number and its price. Must know how to print itself.
class Tool (ID:Int) {
    
    // Tool Decides what it is at random
    private val toolType:String = {
        val random = new Random
        val lis = List("Painting", "Concrete", "Plumbing", "Woodworking", "Yardworking")
        lis(random.nextInt(lis.length))
    }
    
    private val number:Int = ID
    private val price:Double = new ToolBehavior(toolType).price
    private val name:String = toolType + " Tool #" + number.toString
    
    override def toString:String = name + " for $" + price.toString
    def getPrice:Double = price
}

// must know how to price tools based on their type (+ some randomness)
class ToolBehavior (val toolType:String) {
    val price:Double = {
        val random = new Random
        val digit:Double = random.nextInt(10)
        toolType match {
            case "Painting" => digit
            case "Concrete" => digit + 10
            case "Plumbing" => digit + 20
            case "Woodworking" => digit + 30
            case "Yardworking" => digit + 40
        }
    }
}

// responsible for creating the tool inventory for the store and dealing with removing and adding to tool list
class ToolList {
    private var tools:List[Tool] = List()
    for (x <- 0 until 20) {
        tools = tools ++ List(new Tool(x))
    }
    
    def getTools:List[Tool] = {
        return tools
    }
}

// must know what tools are in the rental and when they are due
class Rental (t:List[Tool], d:Int, i:Int, c:Customer){
    private var tools:List[Tool] = t
    private var days:Int = d
    private val ID:Int = i
    private val customer:Customer = c
    private var active:Boolean = true
    private val totalDays:Int = days
    private val totalPrice:Double = tools.foldLeft(0.0)((a, t) => a + t.getPrice) * totalDays
    
    def passDay = {
        days -= 1
        if (days == 0) {
            active = false
        }
    }
    override def toString:String = "RENTAL: " + tools.toString + " for " + totalDays.toString + " days at: $" + totalPrice +" (ID:" + ID.toString + ") " + customer.toString
    def getID:Int = ID
    def getTools:List[Tool] = tools
    def isActive:Boolean = active
}

class RentalControl {
    
    private var usedIDs:Set[Int] = Set()
    private var pastRentals:List[Rental] = List()
    
    def removeRental(cust:Customer, store:Store, rental:Rental):List[Tool] = {
        val ID = rental.getID
        val tools = rental.getTools
        
        cust.relinquishRental(ID)
        //print(store.getRentals)
        store.removeRental(ID)
        unassignID(ID)
        pastRentals = pastRentals ++ List(rental)
        
        return tools
    }
    def addRental(cust:Customer, store:Store, tools:List[Tool], days:Int):Double = {
        if (tools.isEmpty) {
            return 0
        }
        
        val ID = assignID
        
        val rental = new Rental(tools, days, ID, cust)
        //print("RENT\n")
        //print(rental)
        //print("\n")
        
        val price = tools.foldLeft(0.0)((acc, tool) => acc + tool.getPrice)
        
        cust.rentTools(rental)
        store.addRental(rental)
        
        return price * days
    }
    
    def assignID:Int = {
        
        var testID = 0
        var found = false
        
        while(true) {
            if (usedIDs.contains(testID)) {
                testID += 1
            }
            else {
                usedIDs += (testID)
                return testID
            }
        }
        return 0
    }
    
    def unassignID(ID:Int) = {
        usedIDs -= (ID)
    }
    def getPastRentals:List[Rental] = pastRentals
}

// must keep track of current tools in store and current rentals out
class Store {
    
    private var inventory:List[Tool] = {
        val list = new ToolList
        list.getTools
    }
    private var currentRentals:List[Rental] = List()
    private var funds:Double = 0.0
    private val rentalHelper = new RentalControl
    
    def daySimulation(customers:List[Customer]){
        //print("\nTools Rented ---------\n")
        val money = customers.foldLeft(0.0)((acc, cust) => {
            if (cust.getNumRentals < 3 && !inventory.isEmpty ) {
                val numTools = cust.getNumTools
                val days = cust.getNumDays
                
                if (days == 7 && inventory.length < 3) {
                    acc
                }else {
                    val tools:List[Tool] = grabTools(numTools)
                    if (tools.length == 0) {
                        acc
                    }else{
                        acc + rentalHelper.addRental(cust, this, tools, days)
                    }
                }
            }else{
                acc
            }
        })
        
        //print(money)
        //print("\n")
        //print(currentRentals)
        
        funds += money
        
    }
    
    def nightSimulation(customers:List[Customer]){
        customers.map(cust => cust.passDay)
        
        val tools:List[Tool] = customers.foldLeft(List[Tool]())((a, c) => {
            a ++ c.getRentals.foldLeft(List[Tool]())((acc, rent) => if (rent.isActive == false) {
                val ret = rentalHelper.removeRental(c, this, rent)
                acc ++ ret
            }else{
                acc
            })
        })
        
        inventory = inventory ++ tools
        //print("\n------tools taken back------\n")
        //print(tools)
        //print("\n------inven------\n")
        //print(inventory)
    }
    
    private def grabTools(number:Int):List[Tool] = { 
        val tools:List[Tool] = inventory.slice(0,number)
        inventory = inventory.slice(number,inventory.length)
        return tools
    }  
    def addRental(rental:Rental) {
        currentRentals = currentRentals ++ List(rental)
    }
    def removeRental(ID:Int) {
        currentRentals = currentRentals.filter(_.getID != ID)
    }
    
    def getRentals:List[Rental] = currentRentals
    def getInventory:List[Tool] = inventory
    def getFunds:Double = funds
    def getPastRentals:List[Rental] = rentalHelper.getPastRentals
    
}

// Must know name, type, and what rentals they have out
abstract class Customer {
    val name:String
    protected var currentRentals:List[Rental] = List()
    private val numTools:Int = new CustomerBehavior(this).numTools
    private val numDays:Int = new CustomerBehavior(this).numDays
    protected var numRentals:Int = 0
    
    override def toString:String = name
    def getNumDays:Int = numDays
    def getNumTools:Int = numTools
    def getRentals:List[Rental] = currentRentals
    def getNumRentals:Int = numRentals
    
    def relinquishRental(ID:Int) {
        currentRentals = currentRentals.filter(_.getID != ID)
        updateRentalNum
    }
    def rentTools(rental:Rental) {
        currentRentals = currentRentals ++ List(rental)
        updateRentalNum
    }
    def passDay {
        currentRentals.map(rent => rent.passDay)
    }
    def updateRentalNum {
        numRentals = currentRentals.foldLeft(0)((acc, r) => r.getTools.length + acc)
    }
    //def pay(money:Double):Double = money //?? MAYBE ??
}

class BusinessCustomer (n:String) extends Customer {
    override val name = n
    override def toString:String = name + " (Business)" // + numTools.toString
}

class CasualCustomer (n:String) extends Customer {
    override val name = n
    override def toString:String = name + " (Casual)"
}

class RegularCustomer (n:String) extends Customer {
    override val name = n
    override def toString:String = name + " (Regular)"
}

class CustomerControl [+T <: Customer] {
    
    private val customerList:List[Customer] = List(new BusinessCustomer("Mark"), 
                                                   new RegularCustomer("Kevin"), 
                                                   new RegularCustomer("John Mulaney"), 
                                                   new CasualCustomer("Sofia"),
                                                   new BusinessCustomer("Anna"),
                                                   new BusinessCustomer("Solid Snake"),
                                                   new CasualCustomer("Henry IV"),
                                                   new RegularCustomer("Kate"),
                                                   new CasualCustomer("Bucket"),
                                                   new CasualCustomer("Iron Man"))
    
    //print(customerList)
    
    def someCustomers:List[Customer] = {
        val random = new Random
        val r = random.nextInt(6)
        val x1 = random.shuffle(customerList)
        val x2 = x1.slice(0,r)
        //print(x2)
        return x2
    }
    
    def getCustomers:List[Customer] = customerList
    
}

class CustomerBehavior (cust:Customer) {
    var numTools:Int = 0
    var numDays:Int = 0
    
    cust match {
        case cust:RegularCustomer => {
            numTools = {
                val random = new Random
                random.nextInt(3) + 1
            }
            numDays = {
                val random = new Random
                random.nextInt(3) + 3
            }
        }
        case cust:BusinessCustomer => {
            numTools = 3
            numDays = 7
        }
        case cust:CasualCustomer => {
            numTools = {
                val random = new Random
                random.nextInt(2) + 1
            }
            numDays = {
                val random = new Random
                random.nextInt(2) + 1
            }
        }
    }
}

class PrintList (l:List[Any]) {
    override def toString = l.foldLeft("") ((acc, i) => acc + "\n" + i.toString)
}

class Controller {
    private val store = new Store
    private val customers = new CustomerControl
    
    private var days = 35
    
    while(days > 0) {
        //print("DAY" + days.toString + "OOOOOOOOOOOOOOOOOOOOOO\n")
        days -= 1
        store.daySimulation(customers.someCustomers)
        
        if (days != 1) {
            store.nightSimulation(customers.getCustomers)
        }
    }
    
    print("INVENTORY\n")
    print(new PrintList(store.getInventory))
    
    print("\n\nFUNDS\n")
    print(store.getFunds)
    
    print("\n\nCURRENT RENTALS\n")
    print(new PrintList(store.getRentals))
    
    print("\n\nPAST RENTALS\n")
    print(new PrintList(store.getPastRentals))
}

//val tool = new ToolList

//val tools = List(new Tool(1), new Tool(2), new Tool(3), new Tool(4), new Tool(5), new Tool(6))

//val custList = List(new BusinessCustomer("Mark"), new RegularCustomer("Ned"))

//val store = new Store(tools)
//store.daySimulation(custList)
//store.nightSimulation(custList)

//val cL = new CustomerControl

//cL.someCustomers

val c = new Controller

