#include <cstdio>

int main(int argc, char const* argv[])
{
  int a, b;

  scanf("%d %d", &a, &b);

  if (a * b % 2) {
    printf("Yes\n");
  } else {
    printf("No\n");
  }
  return 0;
}
