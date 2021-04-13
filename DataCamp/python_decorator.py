# for i in range(1,3):
#     print('hello!')

# class BaseLine:
#     def __init__(self, name):
#         self.name = name
    
#     def show_name(self):
#         print(self.name)

# line = BaseLine('line')
# line.show_name()


# Decorator
from functools import wraps
import time

def add_hello(func):
  # Decorate wrapper() so that it keeps func()'s metadata
  @wraps(func)
  def wrapper(*args, **kwargs):
    """Print 'hello' and then call the decorated function."""
    print('Hello')
    return func(*args, **kwargs)
  return wrapper

def timeit(func):
  # Decorate wrapper() so that it keeps func()'s metadata
  @wraps(func)
  def wrapper(*args, **kwargs):
    print(f'Start timeing for {func.__name__}')
    start_time = time.time()
    func(*args, **kwargs)
    laps_time = time.time() - start_time
    print(f'{func.__name__} lapse for {laps_time} min.')
  return wrapper

@add_hello
def print_sum(a, b):
  """Adds two numbers and prints the sum"""
  print(a + b)

@timeit
def loop_list(xs):
  """loop list of xs"""
  total = 0
  print(f'Looping : {xs:,}')
  for x in range(1, xs):
    total = total + (x^2)
  return total

def main():
    # print_sum(10, 20)
    # print(print_sum.__doc__)
    loop_list(10000000)

if __name__ == '__main__':
    main()