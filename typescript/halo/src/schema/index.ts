import {readFile} from 'fs-promise'
import YAML from 'yaml';

type SingleParam = number | string
type MultiParam = SingleParam[]
type Param = SingleParam | MultiParam

interface API {
    method: string
    url: string
    async: boolean
}

interface GetAPI extends API {
    method: "GET"
    params?: Map<string, Param>
}

interface PostAPI extends API {
    method: "POST"
    data?: Map<string, any>
}

interface PutAPI extends API {
    method: "PUT"
    data?: Map<string, any>
}

interface APISet {
    apis: Map<string, API>
}

class Schema {
    apis: Map<string, API|APISet>
}

function parse(schema: string): Schema {
    let yaml = YAML.parse(schema)
    console.log(yaml)
    return undefined
}

async function parseFile(path: string): Promise<Schema> {
    let content = await readFile(path, "utf-8")
    return parse(content);
}

parseFile("tests/fixtures/sample.yaml")