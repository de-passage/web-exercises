#include "ghost_in_the_cell.hpp"

using namespace std;
using namespace gitc;

int main() {
  graph map{parse_map(std::cin)};
  factory_container factories;
  troop_container troops;
  bomb_container bombs;

  // game loop
  while (1) {
    troops.clear();
    bombs.clear();
    parse_and_update_entities(std::cin, troops, factories, bombs);
    auto d = decide(map, factories, troops, bombs);
    cerr << "DECISIONS: " << d << endl;
    cout << d << endl;
  }
}