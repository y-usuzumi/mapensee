import {parseFile} from "./schema/parser";

(async () => {
    await parseFile("tests/fixtures/sample.yaml");
})();
