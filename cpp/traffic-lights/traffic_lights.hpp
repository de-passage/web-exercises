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
  int count{0};
};

using info_container = vector<light_data>;

int time_to_destination(int distance, int speed) {
  return static_cast<int>(
      floor(static_cast<float>(distance) / (static_cast<float>(speed) / 3.6)));
}

float speed_to_destination(const light_data& data) {
  return static_cast<float>(data.distance) /
         (static_cast<float>(data.duration) * static_cast<float>(data.count) *
          2.F) *
         3.6F;
}

bool can_get_through(int speed, const light_data& d) {
  int ttd = time_to_destination(d.distance, speed);
  int totdur = d.duration * (2 * d.count + 1);

  return ttd <= totdur;
}

int next_fastest_speed(int max_speed, light_data& data) {
  int speed = max_speed;
  do {
    ++data.count;
    speed = static_cast<int>(speed_to_destination(data));
  } while (speed >= max_speed || !can_get_through(speed, data));
  return speed;
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

int compute_speed_limit(int speed, info_container& lights) {
  int blocker;
  while ((blocker = blocked_by_index(speed, lights)) != -1) {
    speed = next_fastest_speed(speed, lights[static_cast<size_t>(blocker)]);
  }
  return speed;
}
