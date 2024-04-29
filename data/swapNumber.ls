main {
    var bool flg = false;
    var int x = 5;
    var int y = 7;
    print("Before Swapping...");
    print("x = ");
    print(x);
    print("y = ");
    print(y);
    var int temp = x;
    x = y;
    y = temp;
    flg = true;
    print("After Swapping...");
    print("x = ");
    print(x);
    print("y = ");
    print(y);
}