BeginPackage["GraphUtils`"]

graphDiameter::usage = "graphDiameter[graph_Association] returns {diameter, timesAttained}. \
graph must have keys \"n\" (vertex count), \"edges\" (list of 0-indexed {a,b} pairs, \
matching C++'s Edge set), and \"isDirected\" (True/False). Mirrors Graph::diameter, \
including its non-standard while(wasChange)-driven all-pairs shortest path sweep.";

graphToString::usage = "graphToString[graph_Association] returns a string matching \
Graph::toString's exact output format, including the per-edge trailing newlines and \
comma-on-its-own-line separators.";

Begin["`Private`"]

graphDiameter[graph_Association] := Module[
  {n, edges, isDirected, A, wasChange, best, s, timesAttained2, ret},

  n = graph["n"];
  edges = graph["edges"];        (* list of {a, b}, 0-indexed to mirror C++ *)
  isDirected = graph["isDirected"];

  (* Initialize A[i][j] = n for all i,j - matches C++'s use of n as an
     "infinity" sentinel (unreachable pairs can never actually need a
     path of length n in an n-vertex graph, so it works as +infinity). *)
  A = ConstantArray[n, {n, n}];

  (* Set edge weights to 1. C++ uses 0-indexed vertex labels directly as
     array indices; here we add 1 to land in Wolfram's 1-indexed arrays,
     but the vertex labels themselves are untouched. *)
  Do[
    (
      A[[edge[[1]] + 1, edge[[2]] + 1]] = 1;
      If[!isDirected, A[[edge[[2]] + 1, edge[[1]] + 1]] = 1]
    ),
    {edge, edges}
  ];

  (* Zero the diagonal - done AFTER edge assignment, same order as C++,
     so a self-loop edge (a,a) would be overwritten back to 0 here too. *)
  Do[A[[i, i]] = 0, {i, 1, n}];

  (* Repeated full (i,j,k) relaxation sweep, driven by wasChange, exactly
     as in the C++ code. NOTE: this is NOT standard Floyd-Warshall (which
     has k as the outermost loop and needs only one pass) - it's a less
     efficient variant that repeats the whole i/j/k sweep until nothing
     changes. Preserved here intentionally to match the original logic. *)
  wasChange = True;
  While[wasChange,
    wasChange = False;
    Do[
      (
        best = A[[i, j]];
        Do[
          (
            s = A[[i, k]] + A[[k, j]];
            If[s < best,
              best = s;
              wasChange = True;
            ]
          ),
          {k, 1, n}
        ];
        A[[i, j]] = best;
      ),
      {i, 1, n}, {j, 1, n}
    ]
  ];

  (* Find the max entry in A and count how many (i,j) pairs attain it *)
  timesAttained2 = 0;
  ret = 0;
  Do[
    (
      If[A[[i, j]] > ret,
        ret = A[[i, j]];
        timesAttained2 = 0;
      ];
      If[A[[i, j]] == ret,
        timesAttained2++;
      ]
    ),
    {i, 1, n}, {j, 1, n}
  ];

  {ret, timesAttained2}
];

graphToString[graph_Association] := Module[{n, edges, m, lines, body},
  n = graph["n"];
  edges = graph["edges"];
  m = Length[edges];

  lines = Table["(" <> ToString[edge[[1]]] <> "," <> ToString[edge[[2]]] <> ")", {edge, edges}];

  (* "\n,\n" reproduces: edge-line, newline, comma, newline, next-edge-line *)
  body = If[m > 0, StringRiffle[lines, "\n,\n"], ""];

  If[m > 0,
    "(" <> ToString[n] <> ",{\n" <> body <> "\n}\n",
    "(" <> ToString[n] <> ",{\n}\n"
  ]
];

End[]
EndPackage[]
