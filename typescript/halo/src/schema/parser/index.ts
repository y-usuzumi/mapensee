import {isNull} from "halo/utils";
import {readFile} from "mz/fs";
import YAML from "yaml";
import {API, APISet, GetAPI, IContext, PostAPI, PutAPI, Schema} from "../types";

interface ISchemaNode {
    $url?: string;
    $method?: string;
    $data?: {[key: string]: string};
    $params?: {[key: string]: string};
    [key: string]: any;
}

enum NodeType {
    API,
    APISet,
}

function getNodeType(k: string): NodeType {
    if (k.startsWith("~")) {
        return NodeType.APISet;
    }
    return NodeType.API;
}

function parseSchema(node: ISchemaNode): Schema {
    const url = node.$url;
    const ctx = Object.assign({}, node, {super: null});
    const schema = new Schema(url, ctx);
    // tslint:disable-next-line:forin
    for (let k in node) {
        const subNode = node[k] as ISchemaNode;
        switch (getNodeType(k)) {
            case NodeType.APISet:
            k = k.slice(1);
            schema.addAPI(k, parseAPISet(subNode, ctx));
            break;
            case NodeType.API:
            schema.addAPI(k, parseAPI(subNode, ctx));
            break;
        }
    }
    return schema;
}

function parseAPISet(node: ISchemaNode, context: IContext): APISet {
    const url = node.$url;
    const ctx = Object.assign({}, node, {super: context});
    const apiSet = new APISet(url, context);
    // tslint:disable-next-line:forin
    for (let k in node) {
        const subNode = node[k] as ISchemaNode;
        switch (getNodeType(k)) {
            case NodeType.APISet:
            k = k.slice(1);
            apiSet.addAPI(k, parseAPISet(subNode, ctx));
            break;
            case NodeType.API:
            apiSet.addAPI(k, parseAPI(subNode, ctx));
            break;
        }
    }
    return apiSet;
}

function parseAPI(node: ISchemaNode, context: IContext): API {
    const url = node.$url;
    const ctx = Object.assign({}, node, {super: context});
    let api;
    switch (node.$method) {
        case "GET":
        case "get":
        {
            const params = isNull(node.$params) ? {} : node.$params!;
            api = new GetAPI(url, ctx, new Map(Object.entries(params)));
            break;
        }
        case "POST":
        case "post":
        {
            const data = isNull(node.$data) ? {} : node.$data!;
            api = new PostAPI(url, ctx, new Map(Object.entries(data)));
            break;
        }
        case "PUT":
        case "put":
        {
            const data = isNull(node.$data) ? {} : node.$data!;
            api = new PutAPI(url, ctx, new Map(Object.entries(data)));
            break;
        }
        default:
        throw new Error(`Unsupported method: ${node.$method}`);
    }
    return api;
}

export function parse(schema: string): Schema {
    const yaml = YAML.parse(schema) as ISchemaNode;
    return parseSchema(yaml);
}

export async function parseFile(path: string): Promise<Schema> {
    const content = await readFile(path, "utf-8");
    return parse(content);
}
