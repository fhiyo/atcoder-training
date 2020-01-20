#include <iostream>
#include <cstdio>
#include <vector>

using namespace std;

using Graph = vector<vector<int>>;


void dfs(Graph& g, vector<int>& vec, vector<bool>& visited, int v, int num)
{
  visited[v] = true;
  vec[v] = num;
  // printf("v: %i, vec[v]: %i\n", v, vec[v]);
  num++;
  for (const auto& next: g[v]) {
    // printf("next: %i\n", next);
    if (visited[next]) continue;
    dfs(g, vec, visited, next, num);
  }
}

int main(int argc, char const* argv[])
{
  int N, u, v, a, b;

  scanf("%i %i %i", &N, &u, &v);
  // printf("N: %i, u: %i, v: %i\n", N, u, v);

  vector<int> U, V;
  vector<bool> u_visited, v_visited;
  Graph G(N + 1);

  U.assign(N + 1, 0);
  u_visited.assign(N + 1, false);

  V.assign(N + 1, 0);
  v_visited.assign(N + 1, false);

  for (int i = 0; i < N - 1; i++) {
    scanf("%i %i", &a, &b);
    G[a].push_back(b);
    G[b].push_back(a);
  }

  // printf("====\n");
  // printf("G:\n");
  // for (const auto& row: G) {
  //   for (const auto& e: row) {
  //     printf("%i ", e);
  //   }
  //   printf(" EOL\n");
  // }
  // printf("====\n");

  // 初期位置が特殊な状況は別で考える
  // uがleafであり、親のノードにvがいる状況
  if (G[u].size() == 1 and G[u][0] == v) {
    printf("%i\n", 0);
    return 0;
  }

  int ans = 0;

  dfs(G, U, u_visited, u, 0);
  dfs(G, V, v_visited, v, 0);

  // printf("U: ");
  // for (const auto& v: U) {
  //   printf("%i ", v);
  // }
  // printf("\n");
  // printf("V: ");
  // for (const auto& v: V) {
  //   printf("%i ", v);
  // }
  // printf("\n");

  for (int i = 0; i < N + 1; i++) {
    if (U[i] < V[i]) {
      ans = V[i] > ans ? V[i] : ans;
    }
  }
  ans--;

  printf("%i\n", ans);

  // それぞれのvertexについて、uからの距離とvからの距離を考える
  // aokiはtakahashiの初期位置からなるべく遠いleafに移動する (aokiの初期位置がrootとする)
  // 「なるべく遠い」、というだけでは実際には途中経路で捕まる恐れがあるので条件を考える
  // どこのleafに行くかさえ求まれば出力は (vからleafまでの距離)-1
  // 途中経路で捕まるというが、別に途中でaokiの方が早くつく地点があるなら目的地にもaokiの方が早くつくので、
  // leafだけ考えればいい、もしくは全ての点においてtakahashiの方がaokiより早くつく点、という条件をつければいい

  // cout << A[0] << " " << B[0] << endl;
  // cout << A[1] << " " << B[1] << endl;

  // printf("====\n");
  // for (const auto& row: G) {
  //   for (const auto& e: row) {
  //     printf("%i ", e);
  //   }
  //   printf(" EOL\n");
  // }
  // printf("====\n");
  //
  // printf("====\n");
  // for (const auto& v: U) {
  //   printf("%i ", v);
  // }
  // printf("\n");
  // printf("====\n");
  //
  // printf("====\n");
  // for (const auto& v: V) {
  //   printf("%i ", v);
  // }
  // printf("\n");
  // printf("====\n");

  return 0;
}

