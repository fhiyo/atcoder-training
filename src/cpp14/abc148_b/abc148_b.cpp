#include <iostream>

int main(int argc, char const* argv[])
{
  int n;
  std::string s, t;

  std::cin >> n;
  std::cin >> s >> t;

  // std::cout << n << std::endl;
  // std::cout << s << ' ' << t << std::endl;

  for (int i = 0; i < n; i++) {
    std::cout << s[i] << t[i];
  }
  std::cout << std::endl;

  return 0;
}
