import {readFile} from "mz/fs";
import YAML from "yaml";
import {isNull} from "./utils";
import {Schema} from "./schema";

interface ISchemaNode {
    $url?: string;
    $method?: string;
    $data?: {[key: string]: string};
    $params?: {[key: string]: string};
}

function _parse(node: ISchemaNode): Schema {
    const rootUrl = node.$url;
    if (isNull(rootUrl)) {
        throw new Error("")
    }
    let schema = new Schema();
    let allKeys = Object.keys()
}

export function parse(schema: string): Schema {
    const yaml = YAML.parse(schema) as ISchemaNode;
    return _parse(yaml);


}

export async function parseFile(path: string): Promise<Schema> {
    const content = await readFile(path, "utf-8");
    return parse(content);
}

(async () => {
    await parseFile("tests/fixtures/sample.yaml");
})();
