using System;
using System.Linq;


namespace StarVariance
{
    public abstract class Animal
    {
        protected abstract int _Id { get; set; }
        public abstract string SpeciesName { get; }

        public int Id { get; protected set; }

        protected void SetId()
        {
            Id = _Id;
            ++_Id;
        }

        protected Animal()
        {
            SetId();
        }

        public void Talk(string something) {
            Console.WriteLine(String.Format("{0}({1}) says {2}", SpeciesName, this.Id, this.Say(something)));
        }

        public abstract string Say(string something);
    }

    public class Cat : Animal
    {
        protected static int _id;
        protected override int _Id
        {
            get { return _id; }
            set { _id = value; }
        }
        protected static string _speciesName;

        static Cat()
        {
            _speciesName = "cat";
        }

        public Cat() : base()
        {
        }

        public override string SpeciesName
        {
            get { return _speciesName; }
        }

        public override string Say(string something) {
            var meow = String.Join(", ", from c in something select "M" + c + "ow~");
            return meow;
        }
    }

    public class Dog : Animal
    {
        protected static int _id;
        protected override int _Id
        {
            get { return _id; }
            set { _id = value; }
        }
        protected static string _speciesName;

        static Dog()
        {
            _speciesName = "dog";
        }

        public Dog() : base()
        {
        }

        public override string SpeciesName
        {
            get { return _speciesName; }
        }

        public override string Say(string something) {
            var bark = String.Join(", ", from c in something select "B" + c + "rk!");
            return bark;
        }
    }
}
