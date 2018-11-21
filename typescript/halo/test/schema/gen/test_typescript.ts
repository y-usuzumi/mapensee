import assert from "assert";
import { parseFile } from "halo/schema/parser";
import { ITSArg, TSClass, TSImport, TSMethod, TypeScriptGen } from "halo/schema/gen/typescript";

describe("TSImport", function () {
    describe("render", function () {
        it("bare imports should work", function () {
            const imp = new TSImport("./mod", null, []);
            console.log(imp.render());
            assert.equal(imp.render(), `import "./mod";`);
        });
        it("importing a default member should work", function () {
            const imp = new TSImport("./mod", "def", []);
            console.log(imp.render());
            assert.equal(imp.render(), `import def from "./mod";`);
        });
        it("importing members should work", function () {
            const imp = new TSImport("./mod", null, ["hello", "world"]);
            console.log(imp.render());
            assert.equal(imp.render(), `import {hello,world} from "./mod";`);
        });
        it("importing members and the default member should work", function () {
            const imp = new TSImport("./mod", "def", ["hello", "world"]);
            console.log(imp.render());
            assert.equal(imp.render(), `import def,{hello,world} from "./mod";`);
        });
    });
});

describe("TSClass", function () {
    describe("render", function () {
        it("frame should be fine", function () {
            const cls = new TSClass("Hello", true);
            const methodGet = new TSMethod("http://www.baidu.com/", "get", "get", null, true, [
                {name: "id", type: "number"},
            ]);
            cls.addMethod(methodGet);
            console.log(cls.render());
        });
    });
});

describe("TypeScriptGen", function () {
    describe("render", function () {
        it("code should render fine", async function () {
            const schema = await parseFile("test/fixtures/sample.yaml");
            const gen = new TypeScriptGen(schema, "/some/path");
            await gen.render();
        });
    });
});
