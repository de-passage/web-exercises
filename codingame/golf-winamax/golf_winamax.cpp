#include <gtest/gtest.h>
#include "./golf_winamax.hpp"

#include <set>
#include <sstream>

using ss = std::stringstream;
using std::endl;
using std::make_pair;
using std::unordered_set;

constexpr ball operator""_b(unsigned long long c) {
  return ball{static_cast<int>(c)};
}

static_assert(0_b == ball{0},
              "Should produce balls with the right amount of strikes left");
static_assert(1_b == ball{1},
              "Should produce balls with the right amount of strikes left");
static_assert(9_b == ball{9},
              "Should produce balls with the right amount of strikes left");

TEST(Field, ShouldParseTrivialExample) {
  ss input("2 1\n1H");
  auto f = parse_field(input);

  ASSERT_TRUE(f.at(0, 0) == ball{1});
  ASSERT_TRUE(f.at(0, 1) == hole);
}

TEST(Field, ShouldParseMoreComplicatedExample) {
  ss input;
  input << "2.X" << endl;
  input << "..H" << endl;
  input << ".H1" << endl;

  field f(3, 3);
  input >> f;

  for (size_t i = 0; i < 3; ++i)
    for (size_t j = 0; j < 3; ++j) {
      auto c = f.at(i, j);
      if (i == 0 && j == 0) {
        ASSERT_EQ(c, ball{2});
      }
      else if (i == 2 && j == 2) {
        ASSERT_EQ(c, ball{1});
      }
      else if (i == 0 && j == 2) {
        ASSERT_EQ(c, water);
      }
      else if ((i == 1 && j == 2) || (i == 2 && j == 1)) {
        ASSERT_EQ(c, hole);
      }
      else {
        ASSERT_EQ(c, empty);
      }
    }
}

TEST(Cell, BasicEqualityResults) {
  ASSERT_EQ(cell{empty}, empty);
  ASSERT_EQ(cell{water}, water);
  ASSERT_EQ(cell{hole}, hole);
  ASSERT_EQ(cell{ball{2}}, ball{2});

  ASSERT_NE(cell{empty}, water);
  ASSERT_NE(cell{ball{1}}, hole);
  ASSERT_NE(cell{ball{1}}, ball{2});
  ASSERT_NE(cell{water}, ball{1});
}

TEST(AnswerCell, BasicEqualityResults) {
  ASSERT_EQ(answer_cell{empty}, empty);
  ASSERT_EQ(answer_cell{up}, up);
  ASSERT_EQ(answer_cell{down}, down);
  ASSERT_EQ(answer_cell{left}, left);
  ASSERT_EQ(answer_cell{right}, right);

  ASSERT_NE(answer_cell{left}, up);
  ASSERT_NE(answer_cell{up}, down);
  ASSERT_NE(answer_cell{right}, left);
  ASSERT_NE(answer_cell{down}, right);
  ASSERT_NE(answer_cell{empty}, up);
  ASSERT_NE(answer_cell{right}, empty);
}

TEST(Answer, SerializesProperly) {
  ss out;
  answer a(3, 3);
  a.at(0, 0) = down;
  a.at(1, 0) = down;
  a.at(2, 0) = right;
  a.at(2, 2) = up;

  out << a;
  ASSERT_EQ(out.str(), "v..\nv..\n>.^\n");
}

TEST(Answer, EqualiyShouldBehaveProperly) {
  ss in;
  in << "v.." << endl;
  in << "v.." << endl;
  in << ">.^" << endl;
  answer a1(3, 3), a2(3, 3);

  in >> a1;
  a2.at(0, 0) = down;
  a2.at(1, 0) = down;
  a2.at(2, 0) = right;
  a2.at(2, 2) = up;

  ASSERT_EQ(a1, a2);

  a2.at(1, 1) = left;
  ASSERT_NE(a1, a2);
}

TEST(Field, EqualityShouldBehaveProperly) {
  ss input;
  input << "2.X" << endl;
  input << "..H" << endl;
  input << ".H1" << endl;
  field f1(3, 3);
  input >> f1;
  field f2(3, 3);
  f2.at(0, 0) = ball{2};
  f2.at(0, 2) = water;
  f2.at(1, 2) = hole;
  f2.at(2, 1) = hole;
  f2.at(2, 2) = ball{1};

  ASSERT_EQ(f1, f2);

  f2.at(2, 2) = water;
  ASSERT_NE(f1, f2);
}

TEST(Solve, ShouldPassTrivialTest) {
  ss input("2 1\n1H");
  auto f = parse_field(input);
  input.str(">.");
  input.clear();
  answer a(2, 1);
  input >> a;

  ASSERT_EQ(solve(f), a);
}

TEST(Solve, ShouldPassExample) {
  ss input("3 3\n2.x\n..H\n.H1");
  auto f = parse_field(input);
  input.str("v..\nv..\n>.^");
  input.clear();
  answer a(3, 3);
  input >> a;

  ASSERT_EQ(solve(f), a);
}

TEST(Solve, ShouldPassModeratelyComplexExample) {
  ss in;
  in << "4..XX" << endl;
  in << ".H.H." << endl;
  in << "...H." << endl;
  in << ".2..2" << endl;
  in << "....." << endl;
  field f(5, 5);
  in >> f;

  in.str("");
  in.clear();
  in << "v...." << endl;
  in << "v...<" << endl;
  in << "v^..^" << endl;
  in << "v^.^^" << endl;
  in << ">>>^." << endl;
  answer a(5, 5);
  in >> a;

  ASSERT_EQ(solve(f), a);
}

TEST(Solve, ShouldPassMoreComplicatedExample) {
  ss in;
  in << "6 6" << endl;
  in << "3..H.2" << endl;
  in << ".2..H." << endl;
  in << "..H..H" << endl;
  in << ".X.2.X" << endl;
  in << "......" << endl;
  in << "3..H.." << endl;
  field f = parse_field(in);

  in.str("");
  in.clear();
  in << ">>>..v" << endl;
  in << ".>>>.v" << endl;
  in << ">>...." << endl;
  in << "^..v.." << endl;
  in << "^..v.." << endl;
  in << "^....." << endl;

  answer a(6, 6);
  in >> a;
  ASSERT_EQ(solve(f), a);
}

TEST(FindPaths, ShouldFindPathInTrivialCase) {
  ss in("2 1\nH1");
  auto f = parse_field(in);
  answer a(2, 1);
  auto l = find_path({0, 1}, {0, 0}, 1_b, f, a);
  ASSERT_EQ(l.size(), 1);
  path p = l.front();
  ASSERT_EQ(p.size(), 1);
  ASSERT_EQ(p, path{make_pair(left, 1_b)});
}

using path_solutions = std::set<path>;

void show_solution(const path& s) {
  for (auto& p : s) {
    std::cerr << "{" << p.first << ", " << p.second << "}, ";
  }
  std::cerr << endl;
}
void check_solutions(path_solutions& solutions, const path_list& l) {
  for (auto& s : l) {
    auto it = solutions.find(s);
    if (it == solutions.end()) {
      std::cerr << "Invalid solution returned: " << std::endl;
      show_solution(s);
      for (auto& s2 : solutions) {
        std::cerr << "Potential solution: ";
        show_solution(s2);
      }
      FAIL();
    }
    else {
      solutions.erase(it);
    }
  }
  if (solutions.size() > 0) {
    std::cerr << "Unexpected solutions found: " << std::endl;
    for (auto& p : solutions) {
      show_solution(p);
    }
  }
}

TEST(FindPaths, ShouldFindAllPathsInSimpleCase) {
  ss in;
  in << "4 4" << endl;
  in << "3..." << endl;
  in << "...." << endl;
  in << "..H." << endl;
  in << "...." << endl;
  auto f = parse_field(in);
  answer a(4, 4);
  auto l = find_path({0, 0}, {2, 2}, 3_b, f, a);
  ASSERT_EQ(l.size(), 2);

  path_solutions solutions;
  solutions.emplace(
      path{make_pair(right, 3_b), make_pair(down, 2_b), make_pair(left, 1_b)});
  solutions.emplace(
      path{make_pair(down, 3_b), make_pair(right, 2_b), make_pair(up, 1_b)});

  check_solutions(solutions, l);
}

TEST(FindPaths, ShouldAvoidFallingIntoWater) {
  ss in;
  in << "4 4" << endl;
  in << "3..X" << endl;
  in << "...." << endl;
  in << "..H." << endl;
  in << "...." << endl;
  auto f = parse_field(in);
  answer a(4, 4);
  auto l = find_path({0, 0}, {2, 2}, 3_b, f, a);
  ASSERT_EQ(l.size(), 1);

  path_solutions solutions = {
      path{make_pair(down, 3_b), make_pair(right, 2_b), make_pair(up, 1_b)}};
  check_solutions(solutions, l);
}

TEST(FindPaths, ShouldAvoidJumpingOverHole) {
  ss in;
  in << "4 4" << endl;
  in << "3..." << endl;
  in << "...H" << endl;
  in << "..H." << endl;
  in << "...." << endl;
  auto f = parse_field(in);
  answer a(4, 4);
  auto l = find_path({0, 0}, {2, 2}, 3_b, f, a);
  ASSERT_EQ(l.size(), 1);

  path_solutions solutions = {
      path{make_pair(down, 3_b), make_pair(right, 2_b), make_pair(up, 1_b)}};
  check_solutions(solutions, l);
}

TEST(Intersects, ShouldReturnFalseIfNoIntersection) {
  // >>>>>v
  // .....v
  // ..>..v
  // ..^..v
  // ..^<<<
  path p = {make_pair(right, 5_b),
            make_pair(down, 4_b),
            make_pair(left, 3_b),
            make_pair(up, 2_b),
            make_pair(right, 1_b)};
  for (coordinates c : {coordinates{1, 0},
                        {1, 1},
                        {1, 2},
                        {1, 4},
                        {2, 0},
                        {2, 1},
                        {2, 4},
                        {3, 0},
                        {3, 1},
                        {3, 3},
                        {3, 4},
                        {4, 0},
                        {4, 1}}) {
    if (intersects(c, p, coordinates{0, 0})) {
      std::cerr << "{" << x(c) << ", " << y(c) << "} should not intersect."
                << endl;
      FAIL();
    }
  }
}

TEST(Intersects, ShouldReturnTrueIfIntersects) {
  // >>>>>v
  // .....v
  // ..>..v
  // ..^..v
  // ..^<<<
  path p = {make_pair(right, 5_b),
            make_pair(down, 4_b),
            make_pair(left, 3_b),
            make_pair(up, 2_b),
            make_pair(right, 1_b)};
  for (coordinates c : {coordinates{0, 0},
                        {0, 1},
                        {0, 2},
                        {0, 3},
                        {0, 4},
                        {0, 5},
                        {1, 5},
                        {2, 2},
                        {2, 3},
                        {2, 5},
                        {3, 2},
                        {3, 5},
                        {4, 2},
                        {4, 3},
                        {4, 4},
                        {4, 5}}) {
    if (!intersects(c, p, coordinates{0, 0})) {
      std::cerr << "{" << x(c) << ", " << y(c) << "} should intersect." << endl;
      FAIL();
    }
  }
}

TEST(WritePath, ShouldWritePathCorrectlyWithWritingFunction) {
  path p = {make_pair(right, 5_b),
            make_pair(down, 4_b),
            make_pair(left, 3_b),
            make_pair(up, 2_b),
            make_pair(right, 1_b)};
  answer a{6, 5};

  write_path(a, p, {0, 0}, write_path_direction);

  ss in;
  in << ">>>>>v" << endl;
  in << ".....v" << endl;
  in << "..>..v" << endl;
  in << "..^..v" << endl;
  in << "..^<<<" << endl;

  ss answ;
  answ << a;

  ASSERT_EQ(in.str(), answ.str());
}

TEST(WritePath, ShouldOnlyOverwriteGivenPathWithErasingFunction) {
  path p = {make_pair(right, 5_b),
            make_pair(down, 4_b),
            make_pair(left, 3_b),
            make_pair(up, 2_b),
            make_pair(right, 1_b)};
  answer a{6, 5};
  ss in;
  in << ">>>>>v" << endl;
  in << ".>>v.v" << endl;
  in << ".^>v.v" << endl;
  in << ".^^v.v" << endl;
  in << ".^^<<<" << endl;
  in >> a;

  answer expected{6, 5};
  ss exp;
  exp << "......" << endl;
  exp << ".>>v.." << endl;
  exp << ".^.v.." << endl;
  exp << ".^.v.." << endl;
  exp << ".^...." << endl;
  exp >> expected;

  write_path(a, p, {0, 0}, write_empty);

  ASSERT_EQ(a, expected);
}

TEST(Solve, ShouldReturnTheCorrectAnswerForTest6) {
  ss in;
  in << "8 8" << endl;
  in << ".......4" << endl;
  in << "....HH.2" << endl;
  in << "..5....." << endl;
  in << "H....22X" << endl;
  in << ".3XH.HXX" << endl;
  in << "..X3.H.X" << endl;
  in << "..XH...." << endl;
  in << "H2X.H..3" << endl;

  answer a(8, 8);
  ss an;
  an << "v<<<<<<<" << endl;
  an << "v>>>..<<" << endl;
  an << "v^>>>>>v" << endl;
  an << ".^.v<<vv" << endl;
  an << ".^..>.vv" << endl;
  an << "v<<<^.<v" << endl;
  an << "v...^<<<" << endl;
  an << ".>>^.<<<" << endl;
  an >> a;
  ASSERT_EQ(a, solve(parse_field(in)));
}

TEST(Solve, ShouldReturnTheCorrectAnswerForTest8) {
  ss in;
  in << "8 8" << endl;
  in << "H..3..H." << endl;
  in << "......5." << endl;
  in << ".XXXXX.2" << endl;
  in << "3X.H.X2." << endl;
  in << ".XXXXX.H" << endl;
  in << ".....H.." << endl;
  in << "H..3..H." << endl;
  in << ".2..H..3" << endl;

  ss an;
  answer a(8, 8);
  an << ".<<<...<" << endl;
  an << ".v<<<<<^" << endl;
  an << ".v.....^" << endl;
  an << "vv..<.v." << endl;
  an << "vv..^.v." << endl;
  an << "v>>>^.<^" << endl;
  an << "...>>>.^" << endl;
  an << ".>>>...^" << endl;
  an >> a;
  ASSERT_EQ(a, solve(parse_field(in)));
}

TEST(Solve, ShouldReturnTheCorrectAnswerForTest19) {
  ss in;
  in << "40 10" << endl;
  in << "5.....X..3...H................HX.....4XH" << endl;
  in << "......X....XXXXX..............XX..2..HXX" << endl;
  in << "......4H........X..4H.H...3..H....4....." << endl;
  in << ".HH.........H5XX.....H................5." << endl;
  in << "X............XXXX....X.244.2.X..H.5....." << endl;
  in << "X.H..........XXXX.......44...X.........5" << endl;
  in << "..............XX4.......3...H.........3." << endl;
  in << "...3......3..X........X....H.H.........." << endl;
  in << ".......HH.....XXXXX.H.X.......XX....H.XX" << endl;
  in << "3........5....H.H.....X.......HX......XH" << endl;

  // std::cout << solve(parse_field(in)) << endl;
}