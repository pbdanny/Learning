# for i in range(1,3):
#     print('hello!')

class BaseLine:
    def __init__(self, name):
        self.name = name
    
    def show_name(self):
        print(self.name)

line = BaseLine('line')
line.show_name()
