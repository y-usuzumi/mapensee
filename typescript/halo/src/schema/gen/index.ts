import {stat} from "mz/fs";
import {Schema} from "../types";

export abstract class Gen {
    protected readonly _schema: Schema;
    protected readonly _path: string;

    constructor(schema: Schema, path: string) {
        this._schema = schema;
        this._path = path;
    }

    public abstract async render(): Promise<void>;
}
