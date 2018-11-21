import ejs from "ejs";
import { isNull, Nullable } from "halo/utils";
import { Gen } from ".";
import { API, APISet, GetAPI, PostAPI, PutAPI, Schema } from "../types";

interface IRenderable {
    render(): string;
}

export class TSImport implements IRenderable {
    private _path: string;
    private _default: Nullable<string>;
    private _members: string[];

    constructor(path: string, def: Nullable<string>, members: string[]) {
        this._path = path;
        this._default = def;
        this._members = members;
    }

    public render(): string {
        let defaultSect: string;
        let membersSect: string;
        if (!isNull(this._default)) {
            defaultSect = this._default!;
        } else {
            defaultSect = "";
        }
        if (this._members.length > 0) {
            membersSect = `{${this._members.join(",")}}`;
        } else {
            membersSect = "";
        }
        const froms = [];
        if (defaultSect !== "") {
            froms.push(defaultSect);
        }
        if (membersSect !== "") {
            froms.push(membersSect);
        }
        let fromSect: string;
        if (froms.length > 0) {
            fromSect = `import ${froms.join(",")} from `;
        } else {
            fromSect = "import ";
        }
        const importSect = `"${this._path}";`;
        return `${fromSect}${importSect}`;
    }
}

export interface ITSArg {
    name: string;
    type: string;
}

export class TSMethod implements IRenderable {
    private readonly _url: string;
    private readonly _name: string;
    private readonly _axiosFunc: string;
    private readonly _data: any;
    private readonly _isPublic: boolean;
    private readonly _args: ITSArg[];
    private readonly _argsTmpl = ejs.compile(`\
<%_ for (let idx = 0; idx < args.length; idx++) { _%>
    <%_ let arg = args[idx]; -%>
    <%_ if (idx == 0) { -%>
        <%_ %><%- arg.name %>: <%- arg.type -%>
    <%_ } else { -%>
        <%_ %>,<%- arg.name %>: <%- arg.type -%>
    <%_ } _%>
<%_ } _%>\
`);
    private readonly _axiosCallTmpl = ejs.compile(`\
axios.<%- func -%>(<%- args.join(",") -%>)\
`);
    private readonly _tmpl = ejs.compile(`\
<%- isPublic ? "public " : "" %>async <%- name %>(<%- renderedArgs %>): Promise<any> {
return <%- renderedAxiosCall -%>;
}
`);

    constructor(url: string, name: string, axiosFunc: string, data: any, isPublic: boolean = true, args: ITSArg[]) {
        this._url = url;
        this._name = name;
        this._axiosFunc = axiosFunc;
        this._data = data;
        this._isPublic = isPublic;
        this._args = args;
    }

    private renderArgs(): string {
        return this._argsTmpl({
            args: this._args,
        });
    }

    private renderAxiosCall(): string {
        const args = [JSON.stringify(this._url)];
        if (!isNull(this._data)) {
            args.push(JSON.stringify(this._data));
        }
        return this._axiosCallTmpl({
            func: this._axiosFunc,
            args,
        });
    }

    public render(): string {
        return this._tmpl({
            url: this._url,
            name: this._name,
            isPublic: this._isPublic,
            renderedArgs: this.renderArgs(),
            renderedAxiosCall: this.renderAxiosCall(),
        });
    }
}

class TSProp implements IRenderable {
    private readonly _name: string;
    private readonly _apiSetClsName: string;
    private readonly _isPublic: boolean;
    private readonly _tmpl = ejs.compile(`\
<%- isPublic ? "public " : "" %>get <%- name %>() {
    return new <%- apiSetClsName %>(url);
}
`);

    constructor(name: string, apiSetClsName: string, isPublic: boolean = true) {
        this._name = name;
        this._apiSetClsName = apiSetClsName;
        this._isPublic = isPublic;
    }

    public render(): string {
        return this._tmpl({
            name: this._name,
            apiSetClsName: this._apiSetClsName,
            isPublic: this._isPublic,
        });
    }
}

export class TSClass implements IRenderable {
    private readonly _name: string;
    private readonly _isExported: boolean;
    private readonly _methods: TSMethod[] = [];
    private readonly _props: TSProp[] = [];
    private readonly _tmpl = ejs.compile(`\
<%- isExported ? "export " : "" %>class <%-name%> {
<%- renderedMethods %>
<%- renderedProps %>
}
`);

    constructor(name: string, exported: boolean = true) {
        this._name = name;
        this._isExported = exported;
    }

    public addMethod(method: TSMethod) {
        this._methods.push(method);
    }

    public addProp(prop: TSProp) {
        this._props.push(prop);
    }

    public render(): string {
        let renderedMethods = "";
        for (const method of this._methods) {
            renderedMethods += method.render();
        }
        let renderedProps = "";
        for (const prop of this._props) {
            renderedProps += prop.render();
        }
        const result = this._tmpl({
            name: this._name,
            isExported: this._isExported,
            renderedMethods,
            renderedProps,
        });
        return result;
    }
}

class TSCode implements IRenderable {
    private _imports: TSImport[] = [];
    private _classes: TSClass[] = [];

    public addImport(imp: TSImport) {
        this._imports.push(imp);
    }

    public addClass(cls: TSClass) {
        this._classes.push(cls);
    }

    public render(): string {
        let result = "";
        for (const imp of this._imports) {
            result += imp.render();
        }
        for (const cls of this._classes) {
            result += cls.render();
        }
        return result;
    }
}

class Context {

}

export class TypeScriptGen extends Gen {
    private buildAPI(code: TSCode, parentClass: TSClass, key: string, api: APISet | API) {
        switch (api.tag) {
            case "api":
            const axiosFunc = api.method.toLowerCase();
            const params: ITSArg[] = [];
            if (api.method === "GET") {
                const getAPI = api as GetAPI;
                if (!isNull(getAPI.params)) {
                    for (const [k, param] of getAPI.params!) {
                        params.push({name: key, type: param});
                    }
                }
            }
            const apiMethod = new TSMethod(apiMethod, api.url, axiosFunc, );
            break;
            case "apiset":
            let apiSetClsName: string;
            if (!isNull(api.name)) {
                apiSetClsName = api.name!;
            } else {
                apiSetClsName = key.substr(0, 1).toUpperCase() + key.substr(1);
            }
            const apiSetCls = new TSClass(apiSetClsName!, false);
            code.addClass(apiSetCls);
            parentClass.addProp(new TSProp(key, apiSetClsName));
            break;
        }
    }

    public async render(): Promise<void> {
        const code = new TSCode();
        let clsName = this._schema.name;
        if (isNull(clsName)) {
            clsName = "HaloClient";
        }
        let cls = new TSClass(clsName!);
        code.addClass(cls);
        console.log("我的发？");
        for (const [key, api] of this._schema.apis) {

        }
        console.log(code.render());
    }
}
