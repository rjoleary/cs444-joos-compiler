# Joos Compiler Design

Code Generation Design
======================
- Asm
- CodeGenMain vs CodeGenType
For code generation, we have two files generating assembly codes, one is CodeGenMain.hs, containing a function codeGenMain which is used to generate assembly codes for the main file which gives
1. The entrypoint which is the global symbol _start and initializes which initializes all static fields.
2. Call the test method which as being emphysied in a5 spec should be the first method being called.
3. Exit system call asm code for the return.
a file called output/main.s file is created by calling codeGenMain
The other file is CodeGenType.hs, containing a function called codeGenType which is used to generate assembly codes for all statements and expressions. All 


- Register saving
- Narrowing, widening






Generated Code Layout
======================
#Code Generation Structure
## Class layout
For class, the layout is as following:
* Instanceof table for non-abstract class
* vtable for non-abstract class
* static fields with unique global labels
* methods with unique global labels and the implementation of the method.

## Object 
### Object layout
For object, the layout is as following:
* virtual pointer(vptr) points to vtable
* non-static fields
### Creating a new object
When a new object is created (ie. new expression is called), the assembly codes for it is as following: 
1. Allocate space for object. The size in bytes is the number of arguments(for non-static fields) plus one for vptr then times by 4. Then the size is moved into eax, then __malloc is called to allocate space. The starting address of the object is in eax and pushed into stack to save it for now.
2. A function called memclear is called to initilize space based on spec in JLS.
3. vptr is saved into the first address of the object.
4. Each argument is evaluated and the results are pushed into stack. The ordering is from left to right.
5. By matching arguments types with object name, the constructor label is retrived and after saving registers, constructor is called. 
6. After the returning of contructor setting up, registers are restored and esp is added by  4 times the number of argument. 
7. The starting address of the object is poped in eax.
8. New expression gives back the starting address of the object.

## Methods
### Callee
For methods, we discuss different kind of methods including abstract method, native methods, regular static and non-static methods, and special method - constructor. We have different ways to generate asm codes for each type of method.
 * The overall steps are generate unique global label for the method, then push ebp and mov esp into ebp which create frame for the method. The generate statements inside method. Finally after restoring esp and ebp, return from the method.
 * The convention that caller save registers is used. So method doesn't save these.
 * For constructor, a method node is recognized as constructor if the name of the method is empty. After a constructor is called, after saving previous ebp and moving the previous esp into ebp, the first thing to do is calling its super constructor. Since super constructor is the constructor that has zero parameter, we can just get the super constructor label by using concatenation on method, package and name. Then the dynamic fields are initialized, and constructor body is generated to asm code by generateStatement function. After that esp and ebp are restoring and progrem returns from the constructor. Super constructor is called recursively.
 * Object is the super constructor for all constructors and doesn't have super constructor, we check this special constructor and make sure it doesn't call super constructor.

### Caller
 * Static Method Invocation: all arguments inside caller are evaluated and pushed into stack, by the ordering of from left to right. Then caller saves necessary registers. After that, by matching arguments types with method name and method return type, we get the unique label of the method. By moving the label into eax and calling eax, caller calls method. After returning from callee, the result of calling method is stored in eax. Registers are restored. Esp is added by the number of arguments times four.
 * Dynamic Method Invocation: its procudure is similar to static method invocation, but need this pointer and vtable loopup. We first get `this` pointer and push it into stack. Then arguments are evaluated and and pushed into stack just like what static method invocation does.
 VTABLE PART RYAN `After saving registers, the method is called by doing vtable lookup`
After callee returning, caller restores registers. For dynamic method invocation, esp is added by 4*(1 + number of arguments) becasue of this pointer.

## Arrays
### Layout of an array
The layout for an array is as following:
* Virtual pointer (vptr) which points to the vtable of this class
* Length of the array
* The value of the first element in the array
* The value of the second element in the array
* ....
* The value of the last element in the array
### Creating a new array
The steps to create a new array is similar to create a new object.
1. Get the length of the array.
2. Allocate the space of array by using __malloc. The size of the space in bytes is the length of array plus 2, then times 4. The starting address is in eax.
3. memclear is called to initialize the spaces.
4. The vptr is stored in the first 8 bytes from the array starting address.
5. The length of the array is stored in the second 8 bytes from the array starting address.
6. The new array expression returns the starting address of the space mallocated for the array in eax.
### Accessing an array element
To get access to an element in an array, several steps are executed.
1. Evaluate the lvalue of array, push it into stack as this pointer.
2. Evaluate the index of the array.
3. Do nullcheck to make sure the array object exists.
4. Do bound checking for index
5. Calculate the address based on the index
6. Return the address of the element in eax 


## Strings
### Layout of a string object
String is a special array, the layout is similar to an array:
* Virtual pointer(vptr) points to string class
* vptr to the array of char, each letter in the string is one element in the char array
### Creating a new string 
To create a new string, an object of type string is created. Each letter in string is moved to corresponding char array element.
