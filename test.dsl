{
  // Matrix Transpose
  M := [[1, 2], [3, 4], [5, 6]];
  Print(trans(M));  // [[1,3,5],[2,4,6]]

  // Matrix Determinant
  M2 := [[1.0, 2.0], [3.0, 4.0]];
  Print(det(M2));  // -2.0

  // Matrix Inverse
  Minv := inv(M2);
  Print(Minv);  // [[-2, 1], [1.5, -0.5]]

  // Matrix Multiplication
  A := [[1, 2], [3, 4]];
  B := [[5, 6], [7, 8]];
  Print(A * B);  // [[19,22],[43,50]]

  // Matrix-Vector Product
  v := [2, 3];
  Print(A * v);  // [8.0, 18.0]

  // If-Then-Else
  x := 5;
  if x > 0 then {
    Print(1);
  } else {
    Print(0);
  }

  // For Loop
  sum := 0;
  for i := 1 to 5 {
    sum := sum + i;
  }
  Print(sum);  // 15

  // Vector Magnitude
  v := [3.0, 4.0];
  Print(magnitude(v));  // 5.0

  // While Loop
  count := 3;
  while count > 0 {
    Print(count);
    count := count - 1;
  }
}
