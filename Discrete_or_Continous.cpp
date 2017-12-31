#include <iostream>
#include <math.h>
using namespace std;

int main() {
  double r1 = 1;
  double numSide = 3;
  double r2 = 0;

  for (int i = 0; i < 1000000000; i++) {
      r2 = r1/cos(3.14159265358979323846264338/numSide);
      r1 = r2;
      numSide += 1;
  }

  cout.precision(20);
  cout << r2 << ", " << r1 << endl;

  return 0;
}
