import axios from "axios";
import HaloClientUsersBudgets from "./budgets";

export interface IHaloUsersPayload {
    username: string;
    password: string;
}

export default class HaloClientUsers {
    private url: string;

    public constructor(url) {
        this.url = `${url}/users`;
    }

    public async all(): Promise<any> {
        const ret = await axios.get(this.url);
        return JSON.parse(ret.data);
    }

    public async get(id: number): Promise<number> {
        const ret = await axios.get(`${this.url}/${id}`);
        return JSON.parse(ret.data);
    }

    public async create(data: IHaloUsersPayload): Promise<any> {
        const ret = await axios.post(this.url, data);
        return JSON.parse(ret.data);
    }

    public async update(id: number, data: object): Promise<any> {
        const ret = await axios.put(`${this.url}/${id}`, {data, async: true});
        return JSON.parse(ret.data);
    }

    public get budgets(): HaloClientUsersBudgets {
        return new HaloClientUsersBudgets(this.url);
    }
}
