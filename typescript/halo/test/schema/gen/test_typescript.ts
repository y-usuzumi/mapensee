import { assert } from "chai";
import { parseFile } from "halo/schema/parser";
import { parseParameterizedUrl, ITSArg, TSClass, TSImport, TSMethod, TypeScriptGen } from "halo/schema/gen/typescript";

describe("parseParameterizedUrl", function () {
    it("simple url", function () {
        const url = "http://www.baidu.com";
        const [cleanedUrl, args] = parseParameterizedUrl(url);
        assert.deepEqual(cleanedUrl, `"http://www.baidu.com"`);
        assert.deepEqual(args, []);
        console.log(`${url}\n-> ${cleanedUrl}\n&  ${JSON.stringify(args)}`);
    });
    it("one arg w/o type", function () {
        const url = "http://www.baidu.com/<id>";
        const [cleanedUrl, args] = parseParameterizedUrl(url);
        assert.deepEqual(cleanedUrl, `"http://www.baidu.com/" + id`);
        assert.deepEqual(args, [{name: "id", type: "any"}]);
        console.log(`${url}\n-> ${cleanedUrl}\n&  ${JSON.stringify(args)}`);
    });
    it("one arg with type", function () {
        const url = "http://www.baidu.com/<id:number>";
        const [cleanedUrl, args] = parseParameterizedUrl(url);
        assert.deepEqual(cleanedUrl, `"http://www.baidu.com/" + id`);
        assert.deepEqual(args, [{name: "id", type: "number"}]);
        console.log(`${url}\n-> ${cleanedUrl}\n&  ${JSON.stringify(args)}`);
    });
    it("two args with type", function () {
        const url = "http://www.baidu.com/<id:number>/<wtf:boolean>/";
        const [cleanedUrl, args] = parseParameterizedUrl(url);
        assert.deepEqual(cleanedUrl, `"http://www.baidu.com/" + id + "/" + wtf + "/"`);
        assert.deepEqual(args, [
            {name: "id", type: "number"},
            {name: "wtf", type: "boolean"},
        ]);
        console.log(`${url}\n-> ${cleanedUrl}\n&  ${JSON.stringify(args)}`);
    });
});

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

// describe("TSClass", function () {
//     describe("render", function () {
//         it("frame should be fine", function () {
//             const cls = new TSClass("Hello", true);
//             const methodGet = new TSMethod("http://www.baidu.com/", "get", "get", [], true, [
//                 {name: "id", type: "number"},
//             ]);
//             cls.addMethod(methodGet);
//             console.log(cls.render());
//         });
//     });
// });

describe("TypeScriptGen", function () {
    describe("render", function () {
        it("code should render fine", async function () {
            const schema = await parseFile("test/fixtures/sample.yaml");
            const gen = new TypeScriptGen(schema, "/some/path");
            await gen.render();
        });
    });
});
