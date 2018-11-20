import ejs from "ejs";
import { isNull, Nullable } from "halo/utils";
import { Gen } from ".";
import { Schema } from "../types";

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
<%_ } _%>
\
`);
    private readonly _tmpl = ejs.compile(`\
<%- isPublic ? "public " : "" %>async <%- name %>(<%- renderedArgs %>): Promise<any> {
return axios.get(\`<%- url %>\`);
}\
`);

    constructor(url: string, name: string, isPublic: boolean = true, args: ITSArg[]) {
        this._url = url;
        this._name = name;
        this._isPublic = isPublic;
        this._args = args;
    }

    public render(): string {
        const renderedArgs = this._argsTmpl({
            args: this._args,
        });
        return this._tmpl({
            url: this._url,
            name: this._name,
            isPublic: this._isPublic,
            renderedArgs: renderedArgs,
        })
    }
}

export class TSClass implements IRenderable {
    private readonly _name: string;
    private readonly _isExported: boolean;
    private readonly _methods: TSMethod[] = [];
    private readonly _tmpl = ejs.compile(`\
<%- isExported ? "export " : "" %>class <%-name%> {
<%- renderedMethods %>
}\
`);

    constructor(name: string, exported: boolean) {
        this._name = name;
        this._isExported = exported;
    }

    public addMethod(method: TSMethod) {
        this._methods.push(method);
    }

    public render(): string {
        let renderedMethods = "";
        for (const method of this._methods) {
            renderedMethods += method.render();
        }
        const result = this._tmpl({
            name: this._name,
            isExported: this._isExported,
            renderedMethods: renderedMethods,
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
        for (const cls of this._classes) {
            result += cls.render();
        }
        return result;
    }
}

class Context {

}

export class TypeScriptGen extends Gen {
    public async render(): Promise<void> {
        const code = new TSCode();
        for (const [key, api] of this._schema.apis) {
            switch (api.tag) {
                case "api":
                // code.addClass(new TSClass("api"))
                break;
                case "apiset":
                break;
            }
        }
    }
}
