#include <gtest/gtest.h>

#include "./binary_search.hpp"
#include "./test_gen.hpp"

#include <sstream>

using namespace std;
using ss = stringstream;
using c = coordinates;
using b = box;

TEST(Output, ShouldPrintCoordinatesCorrectly) {
  ss in;
  coordinates c{4, 2};
  in << c;
  ASSERT_EQ(in.str(), "4 2");
}
TEST(Coordinates, BasicEquality) {
  ASSERT_EQ((c{1, 1}), (c{1, 1}));
  ASSERT_EQ((c{9, 1}), (c{9, 1}));
  ASSERT_EQ((c{9, 42}), (c{9, 42}));

  ASSERT_NE((c{1, 1}), (c{2, 2}));
  ASSERT_NE((c{11, 1}), (c{11, 2}));
  ASSERT_NE((c{1, 42}), (c{2, 2}));
}

TEST(Middle, ShouldComputeTheCorrectMiddlePoint) {
  ASSERT_EQ(middle({0, 0}, {4, 4}), (c{2, 2}));
  ASSERT_EQ(middle({0, 0}, {1, 1}), (c{0, 0}));
  ASSERT_EQ(middle({4, 4}, {4, 4}), (c{4, 4}));
  ASSERT_EQ(middle({1, 3}, {8, 5}), (c{4, 4}));
}

TEST(DistanceSquared, ShouldComputeCorrectly) {
  ASSERT_EQ(distance_squared(c{3, 2}, c{7, 5}), 25);
  ASSERT_EQ(distance_squared(c{0, 2}, c{0, 5}), 9);
  ASSERT_EQ(distance_squared(c{5, 2}, c{1, 2}), 16);
}

TEST(RelativeDistance, ShouldReturnProperOrdering) {
  ASSERT_EQ(relative_distance_from({3, 4}, {5, 2}, {1, 2}), ordering::equal);
  ASSERT_EQ(relative_distance_from({4, 4}, {6, 2}, {1, 2}), ordering::lesser);
  ASSERT_EQ(relative_distance_from({3, 3}, {6, 1}, {1, 2}), ordering::greater);
}

TEST(Dividing, ShouldSplitBoxesInHalfCorrectly) {
  b test_box(0, 0, 12, 15);
  ASSERT_EQ(test_box.top_half(), b(0, 0, 12, 7));
  ASSERT_EQ(test_box.bottom_half(), b(0, 7, 12, 15));
  ASSERT_EQ(test_box.left_half(), b(0, 0, 6, 15));
  ASSERT_EQ(test_box.right_half(), b(6, 0, 12, 15));
}

TEST(Dividing, ShouldProduceOtherHalfOfBoxOnDemand) {
  b test_box(2, 4, 14, 17);
  b left = test_box.left_half();
  b right = test_box.right_half();
  b top = test_box.top_half();
  b bottom = test_box.bottom_half();

  ASSERT_EQ(test_box.other_half(left), right);
  ASSERT_EQ(test_box.other_half(right), left);
  ASSERT_EQ(test_box.other_half(top), bottom);
  ASSERT_EQ(test_box.other_half(bottom), top);
}

TEST(Dividing, ShouldProduceValidLines) {
  b test_box(0, 0, 5, 6);
  ASSERT_EQ(test_box.vertical_line_at(3), b(3, 0, 4, 6));
  ASSERT_EQ(test_box.horizontal_line_at(2), b(0, 2, 5, 3));
}

TEST(FakeReferee, ShouldBeConstructibleAndIOAsIntended) {
  coordinates initial{1, 5};
  fake_referee ref(test(12, 13, {3, 2}, initial));
  std::iostream io(&ref);

  size_t w, h, n;
  io >> w >> h >> n;
  ASSERT_EQ(w, 12);
  ASSERT_EQ(h, 13);
  ASSERT_EQ(n, test::default_max_tries);

  coordinates ini2;
  io >> ini2;

  ASSERT_EQ(initial, ini2);

  temperature temp;
  io >> temp;

  ASSERT_EQ(temp, temperature::unknown);

  io << "11 12" << std::endl;
  io >> temp;
  ASSERT_EQ(temp, temperature::cold);

  c t = ref.underlying_test().solution();
  t.x++;
  io << t << std::endl;
  io >> temp;
  ASSERT_EQ(temp, temperature::hot);

  io << ref.underlying_test().current_position() << endl;
  io >> temp;
  ASSERT_EQ(temp, temperature::same);

  io << ref.underlying_test().solution() << endl;
  io >> temp;
  ASSERT_EQ(temp, temperature::found);
}

TEST(Symmetry, ShouldFindASymetricPointInAValidBox) {
  b test_box(0, 0, 10, 12);
  c pos{2, 3};

  auto p = test_box.symmetric_point_boxed(pos);
  ASSERT_EQ(p.first, (c{2, 8}));
  ASSERT_EQ(p.second, b(0, 6, 10, 12));

  auto p2 = p.second.symmetric_point_boxed(p.first);
  ASSERT_EQ(p2.first, (c{7, 8}));
  ASSERT_EQ(p2.second, b(5, 6, 10, 12));

  auto p3 = p2.second.symmetric_point_boxed(p2.first);
  ASSERT_EQ(p3.first, (c{7, 9}));
  ASSERT_EQ(p3.second, b(5, 9, 10, 12));
}

TEST(Symmetry, ShouldFindASymmetryInALine) {
  b b1(1, 1, 5, 2);
  c p1{1, 1};
  c p2{4, 1};
  c p3{2, 1};
  c p4{3, 1};
  ASSERT_EQ(b1.symmetric_point_boxed(p1).first, p2);
  ASSERT_EQ(b1.symmetric_point_boxed(p2).first, p1);
  ASSERT_EQ(b1.symmetric_point_boxed(p3).first, p4);
  ASSERT_EQ(b1.symmetric_point_boxed(p4).first, p3);

  b b2(22, 23, 23, 27);
  c p5{22, 23};
  c p6{22, 24};
  c p7{22, 25};
  c p8{22, 26};
  ASSERT_EQ(b2.symmetric_point_boxed(p5).first, p8);
  ASSERT_EQ(b2.symmetric_point_boxed(p6).first, p7);
  ASSERT_EQ(b2.symmetric_point_boxed(p7).first, p6);
  ASSERT_EQ(b2.symmetric_point_boxed(p8).first, p5);
}

TEST(Symmetry, ShouldFindASymmetryInASquare) {
  b b1(22, 22, 24, 24);
  c p1{22, 22};
  c p2{23, 22};
  ASSERT_EQ(b1.symmetric_point_boxed(p1).first, p2);
  ASSERT_EQ(b1.symmetric_point_boxed(p2).first, p1);

  b b2(21, 21, 24, 24);
  c p3{21, 21};
  c p4{23, 21};
  c p5{22, 21};
  ASSERT_EQ(b2.symmetric_point_boxed(p3).first, p4);
  ASSERT_EQ(b2.symmetric_point_boxed(p4).first, p3);
  ASSERT_EQ(b2.symmetric_point_boxed(p5).first, p5);
}

TEST(BestHalf, ShouldReturnTheLongestSideCutInHalf) {
  b test_box(6, 0, 12, 12);

  b h = test_box.best_half();
  auto e1 = b(6, 0, 12, 6);
  auto e2 = b(6, 6, 12, 12);
  if (h != e1 && h != e2) {
    std::cout << "error: value of h (" << h << ") isn't what's expected:\n";
    std::cout << e1 << " or\n";
    std::cout << e2 << std::endl;
    FAIL();
  }
}

TEST(SearchByRectangles, ShouldSucceed_1) {
  c solution{23, 21};
  test t(24, 24, solution, {22, 13}, 15);
  fake_referee ref(t);
  std::iostream io(&ref);

  ASSERT_EQ(search_by_rectangles(io, io), solution);
}

TEST(SearchByRectangles, ShouldSucceed_2) {
  c solution{0, 1};
  test t(15, 15, solution, {3, 6}, 12);
  fake_referee ref(t);
  std::iostream io(&ref);

  ASSERT_EQ(search_by_rectangles(io, io), solution);
}

TEST(SearchByRectangles, ShouldFindTheAnswerInSimpleCase) {
  coordinates solution{2, 3};
  fake_referee ref(test(12, 12, solution, {5, 5}));
  std::iostream io(&ref);

  auto result = search_by_rectangles(io, io);
  ASSERT_EQ(result, solution);
}

TEST(SearchByRectangles, ShouldSucceed_3) {
  c solution{0, 1};
  test t(8000, 8000, solution, {3200, 2100}, 41);  // Need 31 to pass Codingame
  fake_referee ref(t);
  std::iostream io(&ref);

  ASSERT_EQ(search_by_rectangles(io, io), solution);
}