#include "ghost_in_the_cell.hpp"

using namespace std;
using namespace gitc;

int main() {
  graph map{parse_map()};
  factory_container factories;
  troop_container troops;

  // game loop
  while (1) {
    troops.clear();
    parse_and_update_entities(troops, factories);
    cout << decide(map, factories, troops) << endl;
  }
}