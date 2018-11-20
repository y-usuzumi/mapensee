import axios from "axios";

export default class HaloClientUsersBudgets {
    private url: string;

    public constructor(url: string) {
        this.url = `${url}/budgets`;
    }

    public async all(): Promise<any> {
        const ret = await axios.get(this.url);
        return JSON.parse(ret.data);
    }
}
