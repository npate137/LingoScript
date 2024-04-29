main {
    var int num1 = 7;
    var int num2 = 12;
    var int num3 = 3 + 6;
    const int constantNum = 3 * 6 - 18;
    const bool constantBool = true;
    var bool flag = 3 < 6;
    const bool constantFlag = false;
    for (num3 = 7; num3 < 18; num3 += 3) {
        num1 = num1 + 3;
    }
    for (var int counter = 7; counter < 23; counter += 3) {
        num3 = num3 + 4;
    }
    if (23 == 23) {
        print("counter=23");
    }
    else {
        print("counter!=23");
    }
}
