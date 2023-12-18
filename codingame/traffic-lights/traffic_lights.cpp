#include <gtest/gtest.h>

#include "traffic_lights.hpp"

TEST(TrafficLights, VillageLight) {
  int speed = 50;
  info_container lights{light_data{200, 15}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 50);
}

TEST(TrafficLights, VillageLight2) {
  int speed = 50;
  info_container lights{light_data{200, 10}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 36);
}

TEST(TrafficLights, QuietCountryRoad) {
  int speed = 90;
  info_container lights{light_data{300, 30}, light_data{1500, 30},
                        light_data{3000, 30}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 90);
}

TEST(TrafficLights, LessQuietCountryRoad) {
  int speed = 90;
  info_container lights{light_data{300, 10}, light_data{1500, 10},
                        light_data{3000, 10}};

  ASSERT_EQ(compute_speed_limit(speed, lights), 54);
}

TEST(TrafficLights, DesyncedCountryRoad) {
  int speed = 90;
  info_container lights{light_data{300, 30}, light_data{1500, 20},
                        light_data{3000, 10}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 67);
}

TEST(TrafficLights, AnotherCountryRoad) {
  int speed = 80;
  info_container lights{light_data{700, 25}, light_data{2200, 15},
                        light_data{3000, 10}, light_data{4000, 28}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 49);
}

TEST(TrafficLights, GermanHighway) {
  int speed = 200;
  info_container lights{light_data{1000, 15}, light_data{3000, 10},
                        light_data{4000, 30}, light_data{5000, 30},
                        light_data{6000, 5},  light_data{7000, 10}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 60);
}

TEST(TrafficLights, RainOfLights) {
  int speed = 130;
  info_container lights{
      light_data{500, 15},   light_data{1000, 15},  light_data{1500, 15},
      light_data{2000, 15},  light_data{2500, 15},  light_data{3000, 15},
      light_data{3500, 15},  light_data{4000, 15},  light_data{4500, 15},
      light_data{5000, 15},  light_data{5500, 15},  light_data{6000, 15},
      light_data{6500, 15},  light_data{7000, 15},  light_data{7500, 15},
      light_data{8000, 15},  light_data{8500, 15},  light_data{9000, 15},
      light_data{9500, 15},  light_data{10000, 15}, light_data{10500, 15},
      light_data{11000, 15}, light_data{11500, 15}, light_data{12000, 15},
      light_data{12500, 15}, light_data{13000, 15}, light_data{13500, 15},
      light_data{14000, 15}, light_data{14500, 15}, light_data{15000, 15},
      light_data{15500, 15}, light_data{16000, 15}, light_data{16500, 15},
      light_data{17000, 15}, light_data{17500, 15}, light_data{18000, 15},
      light_data{18500, 15}, light_data{19000, 15}, light_data{19500, 15},
      light_data{20000, 15}, light_data{20500, 15}, light_data{21000, 15},
      light_data{21500, 15}, light_data{22000, 15}, light_data{22500, 15},
      light_data{23000, 15}, light_data{23500, 15}, light_data{24000, 15},
      light_data{24500, 15}, light_data{25000, 15}, light_data{25500, 15},
      light_data{26000, 15}, light_data{26500, 15}, light_data{27000, 15},
      light_data{27500, 15}, light_data{28000, 15}, light_data{28500, 15},
      light_data{29000, 15}, light_data{29500, 15}, light_data{30000, 15},
      light_data{30500, 15}, light_data{31000, 15}, light_data{31500, 15},
      light_data{32000, 15}, light_data{32500, 15}, light_data{33000, 15},
      light_data{33500, 15}, light_data{34000, 15}, light_data{34500, 15},
      light_data{35000, 15}, light_data{35500, 15}, light_data{36000, 15},
      light_data{36500, 15}, light_data{37000, 15}, light_data{37500, 15},
      light_data{38000, 15}, light_data{38500, 15}, light_data{39000, 15},
      light_data{39500, 15}, light_data{40000, 15}, light_data{40500, 15},
      light_data{41000, 15}, light_data{41500, 15}, light_data{42000, 15},
      light_data{42500, 15}, light_data{43000, 15}, light_data{43500, 15},
      light_data{44000, 15}, light_data{44500, 15}, light_data{45000, 15},
      light_data{45500, 15}, light_data{46000, 15}, light_data{46500, 15},
      light_data{47000, 15}, light_data{47500, 15}, light_data{48000, 15},
      light_data{48500, 15}, light_data{49000, 15}, light_data{49500, 15},
      light_data{50000, 15}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 60);
}

TEST(TrafficLights, ChristmasTree) {
  int speed = 130;
  info_container lights{
      light_data{1100, 10},  light_data{1150, 15},  light_data{1200, 20},
      light_data{1250, 25},  light_data{1300, 30},  light_data{2100, 10},
      light_data{2150, 15},  light_data{2200, 20},  light_data{2250, 25},
      light_data{2300, 30},  light_data{3100, 10},  light_data{3150, 15},
      light_data{3200, 20},  light_data{3250, 25},  light_data{3300, 30},
      light_data{4100, 10},  light_data{4150, 15},  light_data{4200, 20},
      light_data{4250, 25},  light_data{4300, 30},  light_data{5100, 10},
      light_data{5150, 15},  light_data{5200, 20},  light_data{5250, 25},
      light_data{5300, 30},  light_data{6100, 10},  light_data{6150, 15},
      light_data{6200, 20},  light_data{6250, 25},  light_data{6300, 30},
      light_data{7100, 10},  light_data{7150, 15},  light_data{7200, 20},
      light_data{7250, 25},  light_data{7300, 30},  light_data{8100, 10},
      light_data{8150, 15},  light_data{8200, 20},  light_data{8250, 25},
      light_data{8300, 30},  light_data{9100, 10},  light_data{9150, 15},
      light_data{9200, 20},  light_data{9250, 25},  light_data{9300, 30},
      light_data{10100, 10}, light_data{10150, 15}, light_data{10200, 20},
      light_data{10250, 25}, light_data{10300, 30}, light_data{11100, 10},
      light_data{11150, 15}, light_data{11200, 20}, light_data{11250, 25},
      light_data{11300, 30}, light_data{12100, 10}, light_data{12150, 15},
      light_data{12200, 20}, light_data{12250, 25}, light_data{12300, 30},
      light_data{13100, 10}, light_data{13150, 15}, light_data{13200, 20},
      light_data{13250, 25}, light_data{13300, 30}, light_data{14100, 10},
      light_data{14150, 15}, light_data{14200, 20}, light_data{14250, 25},
      light_data{14300, 30}, light_data{15100, 10}, light_data{15150, 15},
      light_data{15200, 20}, light_data{15250, 25}, light_data{15300, 30},
      light_data{16100, 10}, light_data{16150, 15}, light_data{16200, 20},
      light_data{16250, 25}, light_data{16300, 30}, light_data{17100, 10},
      light_data{17150, 15}, light_data{17200, 20}, light_data{17250, 25},
      light_data{17300, 30}, light_data{18100, 10}, light_data{18150, 15},
      light_data{18200, 20}, light_data{18250, 25}, light_data{18300, 30},
      light_data{19100, 10}, light_data{19150, 15}, light_data{19200, 20},
      light_data{19250, 25}, light_data{19300, 30}, light_data{20100, 10},
      light_data{20150, 15}, light_data{20200, 20}, light_data{20250, 25},
      light_data{20300, 30}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 6);
}

TEST(TrafficLights, QuickLights) {
  int speed = 90;
  info_container lights{
      light_data{1234, 5},  light_data{2468, 5},  light_data{3702, 5},
      light_data{6170, 5},  light_data{8638, 5},  light_data{13574, 5},
      light_data{16042, 5}, light_data{20978, 5}, light_data{23446, 5},
      light_data{28382, 5}, light_data{35786, 5}, light_data{38254, 5},
      light_data{45658, 5}, light_data{50594, 5}, light_data{53062, 5},
      light_data{57998, 5}};
  ASSERT_EQ(compute_speed_limit(speed, lights), 74);
}
