import { Gen } from "./schema/gen";
import { TypeScriptGen } from "./schema/gen/typescript";
import { parseFile } from "./schema/parser";

(async () => {
    try {
        const schema = await parseFile("tests/fixtures/sample.yaml");
        // console.log(schema);
        const g: Gen = new TypeScriptGen(schema, "/hello/world");
        g.render();
    } catch (e) {
        console.log(e);
    }
})();
