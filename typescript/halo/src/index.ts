import {parseFile} from "./schema/parser";

(async () => {
    try {
        const schema = await parseFile("tests/fixtures/sample.yaml");
        console.log(schema);
    } catch (e) {
        console.log(e);
    }
})();
