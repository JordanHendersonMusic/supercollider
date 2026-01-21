// Largest array possible.
// As PyrSlot is 8 bytes and we use an int32 to store the size (num elements) and the byte size, this means we can only represent 2^28 - 1 values.
(1.. 2.pow(28).asInteger - 1);

0.exit;
