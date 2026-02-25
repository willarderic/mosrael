fn main() {
    var c string;
    var a u8 = 10;
    var b = a + -5 * 2;
    for i = 0; i < 10; i = i + 1 {
        b = b / i;
    }
    if a == 10 {
        return b;
    }

    return ~b;
}
