#include "ghost_in_the_cell.hpp"

using namespace std;
int main() {
  graph map{parse_map()};
  entity_container entities;

  // game loop
  while (1) {
    parse_and_update_entities(entities);
    // Any valid action, such as "WAIT" or "MOVE source destination cyborgs"
    cout << "WAIT" << endl;
  }
}