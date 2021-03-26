#include <algorithm>
#include <cmath>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct light_data {
  constexpr light_data(int dist, int dur) noexcept
      : distance{dist}, duration{dur} {}
  int distance;
  int duration;
};

using info_container = vector<light_data>;

double round3(double d) { return round(d * 1000) / 1000; }

double time_to_destination(int speed, const light_data& data) {
  return round3(static_cast<double>(data.distance) /
                (static_cast<double>(speed) / 3.6));
}

double speed_to_destination(int count, const light_data& data) {
  return round3(
      static_cast<double>(data.distance) /
      (static_cast<double>(data.duration) * static_cast<double>(count * 2)) *
      3.6);
}

bool can_get_through(int speed, const light_data& data) {
  double arrival_time = time_to_destination(speed, data);
  int round_at_arrival = static_cast<int>(arrival_time) / (data.duration * 2);
  int light_turns_green = round_at_arrival * data.duration * 2;
  int light_turns_red = data.duration * (round_at_arrival * 2 + 1);

  return arrival_time >= static_cast<double>(light_turns_green) &&
         arrival_time < static_cast<double>(light_turns_red);
}

int next_fastest_speed(int speed, const light_data& data) {
  int current_round =
      static_cast<int>(time_to_destination(speed, data)) / (data.duration * 2);
  return static_cast<int>(floor(speed_to_destination(current_round + 1, data)));
}

int blocked_by_index(int speed, const info_container& data) {
  for (size_t i = 0; i < data.size(); ++i) {
    const auto& d = data[i];
    if (!can_get_through(speed, d)) {
      return static_cast<int>(i);
    }
  }
  return -1;
}

int compute_speed_limit(int speed, const info_container& lights) {
  int blocker;
  while ((blocker = blocked_by_index(speed, lights)) != -1) {
    speed = next_fastest_speed(speed, lights[static_cast<size_t>(blocker)]);
  }
  return speed;
}
