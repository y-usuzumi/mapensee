var inputs = ["flower", "flow", "flight"]

var longestCommonPrefix = function (strs) {
    if(!strs.length) {
        return "";
    }

    ret = []

    let minLen = Math.min.apply(null, strs.map(i => i.length));

    for (let idx=0; idx<minLen; idx++) {
        let ch = strs[0][idx]
        let match = true;
        for(let input of strs) {
            if (input[idx] != ch) {
                match = false;
                break;
            }
        }
        if (match) {
            ret.push(ch);
        } else {
            break;
        }
    }

    return ret.join('')
}

var longestCommonPrefix2 = (a, b) => {
    if (a.length == 0 || b.length == 0) {
        return ""
    }
    return (
        a[0] == b[0]
        ? a[0] + longestCommonPrefix2(a.substring(1), b.substring(1))
        : ""
    )
}

var longestCommonPrefixRecursive = function (strs) {
    if (strs.length == 0) {
        return ""
    }
    if (strs.length == 1) {
        return strs[0]
    }
    return strs.reduce(longestCommonPrefix2)
}

console.log(longestCommonPrefixRecursive(inputs))
