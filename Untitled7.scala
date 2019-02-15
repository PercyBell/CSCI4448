
// Percy Bell : CSCI 4448 : Febuary 15 2019
// HW#2 part 2

// Abstract shape class
// Shapes are responsible for knowing what they are, where they are and how big they are 
// as well as knowing how to display themselves
abstract class Shape {
    
    // information all shapes must have
    val name: String
    val coords: (Int, Int)
    val z: Int
    
    // a way to get z from other classes
    def getZ: Int = z
    
    // setup for a print method
    def toString: String
}

class Triangle (val coords: (Int, Int), val z: Int, val height: Int) extends Shape {
    
    // overide name for triangles specifically
    override val name: String = "Triangle"
    
    // overide print for triangles specifially
    override def toString: String = name + " at " + coords + "\n"
}

class Square (val coords: (Int, Int), val z: Int, val width: Int) extends Shape {
    
    // overide name for squares specifially
    override val name: String = "Square"
    
    // overide print for squares specifially
    override def toString: String = name + " at " + coords + "\n"
}

class Circle (val coords: (Int, Int), val z: Int, val radius: Int) extends Shape {
    
    // overide name for circles specifially
    override val name: String = "Circle"
    
    // overide print for circles specifially
    override def toString: String = name + " at " + coords + "\n"
}

// collections are responible for keeping track of and sorting shapes by z order
class Collection [+T <: Shape] (C:List[T] = List()) {
    // the list of shapes in collection
    val col: List[Shape] = C
    
    // way to get shapes list
    def getCol: List[Shape] = col
    
    // add to collection but doesnt sort
    def addToCol (s: Shape) = new Collection (col ++ List(s))
    
    // adds and sorts shapes into collection
    def addSort(s: Shape):Collection[Shape] = {
        var ls = List[Shape]()
        
        if (col == List()) {
            ls = List(s)
        }
        else {
            ls = col.foldLeft(List[Shape]())((a, i) => if (s.getZ > i.getZ) {a ++ List(s) ++ List(i)} else  {a ++ List(i)})
            
            if (ls.length == col.length) {
                ls = ls ++ List(s)
            }
        }
        val c:Collection[Shape] = new Collection(ls)
        return c
    }
}

// Displays are responsible for printing shape lists
class Display (val C: Collection[Shape]) {
    // get shape list from collection
    val lst = C.getCol
    
    // for each shape ask it to print itself
    for (x <- 0 until lst.length) {
        print (lst(x).toString)
    }
}

// ceate three shapes
val a = new Triangle((0,1), 10, 5)
val b = new Square((9,13), 4, 1)
val c = new Circle((1,1), 6, 20)

// add the shapes to a collection
val CO = new Collection()
val CO2 = CO.addSort(a)
val CO3 = CO2.addSort(b)
val CO4 = CO3.addSort(c)

// ask for the shapes to be printed
val Disp = new Display(CO4)
