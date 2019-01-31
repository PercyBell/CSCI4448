#
# Percy Bell : Homework #1 (P#4) : CSCI 4448 : 2/1/19
#

# Shapes: Responsible for... Knowing where they are, what they are,
#                   their own dimensions and how to display themselves.
# Collection: Responsible for... Knowing how to sort shapes.
# Display: Responsible for... Printing things to "screen" (AKA terminal).

class Shape:
    ''' Abstract class to define different shapes'''

    def __init__(self, x, y, z):
        # Things all shapes must have:
        self.coords = (x,y) # x and y coordiante of shape
        self.name = None    # What kind of shape
        self.z = z          # z level for dispaly

    def getz(self):
        ''' Gives the z level of the shape'''

        return self.z

    def __repr__(self):
        ''' Explains how shapes must be printed in string format '''
        s = str(self.name) + ': '
        s += 'at ' + str(self.coords)

        return s
        

class Square(Shape):
    ''' Class specificaly for squares'''
    
    def __init__(self, x, y, z, l):

        Shape.__init__(self, x, y, z)
        # Things only squares have:
        self.length = l      # A length and width which are the same
        self.name = 'Square' # The name "Square"

    def __repr__(self):
        ''' Adds to the display str specifically for squares'''

        s = Shape.__repr__(self)

        s += ' of length/width ' + str(self.length)

        return s
    

class Circle(Shape):
    ''' Class specificaly for circles'''
    
    def __init__(self, x, y, z, r):

        Shape.__init__(self, x, y, z)
        # Things only circles have:
        self.radius = r       # a radius
        self.name = 'Circle'  # the name "Circle"

    def __repr__(self):
        ''' Adds to the display str specifically for circles'''

        s = Shape.__repr__(self)

        s += ' of radius ' + str(self.radius)

        return s

class Triangle(Shape):
    ''' Class specificaly for triangles'''
    def __init__(self, x, y, z, h):

        Shape.__init__(self, x, y, z)
        # Things only triangles have:
        self.height = h         # A height
        self.name = 'Triangle'  # The name "Triangle"

    def __repr__(self):
        ''' Adds to the display str specifically for triangles'''

        s = Shape.__repr__(self)

        s += ' of height ' + str(self.height)

        return s

class Collection:
    ''' Class responsible for organizing the shapes by z level'''

    def __init__(self):

        self.lst = [] # a list of shapes initilized to empty

    def getLst(self):
        ''' returns the shape list'''

        return self.lst

    def add(self, item):
        ''' Adds an item to the shape list by z level'''

        z = item.getz()

        if (self.lst == []):
            self.lst += [item]

            return

        for i in range(len(self.lst)):

            if (self.lst[i].getz() <= z):
                self.lst = self.lst[:i] + [item] + self.lst[i:]
                return

        self.lst += [item]



class Display:
    ''' Class responsible for displaying a list of shapes'''

    def __init__(self, col):

        lst = col.getLst()

        print(len(lst), "in list:")

        # calls generic print for each item in list taken from given collection
        for i in lst:
            print(i)


# create some shapes
a = Triangle(2, 3, 4, 10)
b = Circle(2, 5, 10, 2)
c = Square(1, 1, 3, 10)

# create a collection
data = Collection()

# add shapes to collection
data.add(a)
data.add(b)
data.add(c)

# create a display
disp = Display(data)
        
