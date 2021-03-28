#include <code_vs_zombie.hpp>

#include <iostream>

using namespace cvsz;
using namespace std;

int main(int, char**) {
  // game loop
  while (1) {
    int x;
    int y;
    cin >> x >> y;
    cin.ignore();
    int humanCount;
    cin >> humanCount;
    cin.ignore();
    for (int i = 0; i < humanCount; i++) {
    }
    int zombieCount;
    cin >> zombieCount;
    cin.ignore();
    for (int i = 0; i < zombieCount; i++) {
      int zombieId;
      int zombieX;
      int zombieY;
      int zombieXNext;
      int zombieYNext;
      cin >> zombieId >> zombieX >> zombieY >> zombieXNext >> zombieYNext;
      cin.ignore();
    }
    cout << "0 0" << endl;  // Your destination coordinates
  }
  return 0;
}