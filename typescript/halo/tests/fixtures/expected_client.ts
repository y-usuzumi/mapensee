import $http from '我的发';

interface _HaloUsersPayload {
    username: string;
    password: string;
}

class _HaloClient {
    private _url: string;

    constructor(url) {
        this._url = url;
    }

    private _url_users(): string {
        return `${this._url}/users`;
    }

    get users(): _HaloClientUsers {
        return new _HaloClientUsers(this._url);
    }
}

class _HaloClientUsers {
    private _url: string;

    constructor(url) {
        this._url = `${url}/users`;
    }

    private _url_get(id: number): string {
        return `${this._url}/${id}`;
    }

    private _url_update(id: number): string {
        return `${this._url}/${id}/`;
    }

    all(): any {
        let ret = $http.get(this._url, {async: false});
        return JSON.parse(ret);
    }

    async get(id: number): Promise<number> {
        let ret = await $http.get(this._url_get(id), {async: true});
        return JSON.parse(ret);
    }

    async create(data: _HaloUsersPayload): Promise<any> {
        let ret = await $http.post(this._url, {data, async: true});
        return JSON.parse(ret);
    }

    async update(id: number, data: Object): Promise<any> {
        let ret = await $http.put(this._url_update(id), {data, async: true});
        return JSON.parse(ret);
    }

    get budgets(): _HaloClientUsersBudgets {
        return new _HaloClientUsersBudgets(this._url);
    }
}

class _HaloClientUsersBudgets {
    private _url: string

    constructor(url) {
        this._url = `${url}/budgets`;
    }

    async all(): Promise<any> {
        let ret = $http.get(this._url, {async: false});
        return JSON.parse(ret);
    }
}
