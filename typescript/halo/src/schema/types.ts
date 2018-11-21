import {Nullable} from "halo/utils";

export interface IContext {
    super: Nullable<IContext>;
    [key: string]: any;
}

export abstract class API {
    public readonly tag = "api";
    protected readonly _method: string;
    protected _url: Nullable<string>;
    protected _context: IContext;

    constructor(url: Nullable<string>, context: IContext) {
        this._url = url;
        this._context = context;
    }

    public get url(): Nullable<string> {
        return this._url;
    }

    public get context(): IContext {
        return this._context;
    }

    public get method(): string {
        return this._method;
    }
}

export class GetAPI extends API {
    protected readonly _method = "GET";
    protected readonly _params?: Map<string, string>;

    constructor(url: Nullable<string>, context: IContext, params?: Map<string, string>) {
        super(url, context);
        this._params = params;
    }

    public get params(): Nullable<Map<string, string>> {
        return this._params;
    }
}

export class PostAPI extends API {
    protected readonly _method: "POST";
    protected readonly _data?: Map<string, any>;

    constructor(url: Nullable<string>, context: IContext, data?: Map<string, string>) {
        super(url, context);
        this._data = data;
    }

    public get data(): Nullable<Map<string, any>> {
        return this._data;
    }
}

export class PutAPI extends API {
    protected readonly _method: "PUT";
    protected readonly _data?: Map<string, any>;

    constructor(url: Nullable<string>, context: IContext, data?: Map<string, string>) {
        super(url, context);
        this._data = data;
    }

    public get data(): Nullable<Map<string, any>> {
        return this._data;
    }
}

export class APISet {
    public readonly tag = "apiset";
    protected readonly _name: Nullable<string>;
    protected readonly _url: Nullable<string>;
    protected readonly _context: IContext;
    protected readonly _apis: Map<string, API|APISet> = new Map();

    constructor(name: Nullable<string>, url: Nullable<string>, context: IContext) {
        this._name = name;
        this._url = url;
        this._context = context;
    }

    public get name(): Nullable<string> {
        return this._name;
    }

    public get url(): Nullable<string> {
        return this._url;
    }

    public get context(): IContext {
        return this._context;
    }

    public get apis(): Map<string, API|APISet> {
        return this._apis;
    }

    public addAPI(k: string, api: API|APISet) {
        this._apis.set(k, api);
    }
}

export class Schema {
    protected readonly _name: Nullable<string>;
    protected readonly _url: Nullable<string>;
    protected readonly _context: IContext;
    protected readonly _apis: Map<string, API|APISet> = new Map();

    constructor(name: Nullable<string>, url: Nullable<string>, context: IContext) {
        this._name = name;
        this._url = url;
        this._context = context;
    }

    public get name(): Nullable<string> {
        return this._name;
    }

    public get url(): Nullable<string> {
        return this._url;
    }

    public get apis(): Map<string, API|APISet> {
        return this._apis;
    }

    public addAPI(k: string, api: API|APISet) {
        this._apis.set(k, api);
    }
}
