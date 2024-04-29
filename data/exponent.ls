main {
    var int base = 7;
    var int exponent = 0;
    var int result = 1;
    while (exponent < 4) {
        result = result * base;
        exponent ++;
    }
    print("The value of ");
    print(base);
    print(" raised to the power of 4 is ");
    print(result);
}