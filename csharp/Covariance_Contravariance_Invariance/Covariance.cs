using System;
using System.Collections.Generic;


namespace StarVariance
{
    class MainClass
    {
        static void P(string msg) {
            Console.WriteLine(msg);
        }

        static void NewSec(int lvl=0) {
            switch(lvl) {
                case 0:
                    Console.WriteLine();
                    Console.WriteLine("==========");
                    Console.WriteLine();
                    break;
                case 1:
                    Console.WriteLine();
                    Console.WriteLine("----------");
                    Console.WriteLine();
                    break;
                default:
                    Console.WriteLine("-----");
                    break;
            }
        }

        static void Info(string msg) {
            Console.WriteLine("*** " + msg + " ***");
        }

        public static void Main(string[] args)
        {
            NewSec();

            P("Covariance sample code");

            NewSec();  // Covariant array types

            P("Array types are covariant, meaning that " +
              "Cat[] is a subtype of Animal[].");

            try
            {
                P("And thus assigning a value of a different " +
                  "subtype to an array of the supertype is not " +
                  "a compile error but a runtime error."
                  );
                Animal[] animals = new [] {
                    new Cat(),
                    new Cat(),
                };

                animals[0] = new Dog();
            }
            catch (ArrayTypeMismatchException) {
                Info("Caught ArrayTypeMismatchException!!");
            }

            NewSec();  // Invariant list types

            P("List types are invariant, meaning that " +
              "Neither Cat[] is a subtype of Animal[], " +
              "nor vice versa."
              );

            // The following code won't compile:
            // List<Animal> animals = new List<Cat> {
            //     new Cat(),
            //     new Cat(),
            // }

            NewSec();  // Covariant IEnumerable

            P("However, IEnumerables are covariant:");
            P("interface IEnumerable<out T> { ... }");

            {
                IEnumerable<Animal> animals = new List<Cat> {
                    new Cat(),
                    new Cat()
                };

                foreach(var animal in animals) {
                    animal.Talk("Win!");
                }
            }

            NewSec();  // Contravariant Action

            P("Action's are contravariant:");
            P("delegate void Action<in T>(T obj)");

            {
                var cat = new Cat();
                var dog = new Dog();
                Action<Animal> animalAction = (Animal animal) => { animal.Talk("Fight!"); };
                animalAction(dog);
                Action<Cat> catAction = animalAction;
                catAction(cat);
                // The following code won't compile:
                // catAction(dog);
            }
        }
    }
}
