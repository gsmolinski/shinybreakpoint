function (cellInfo) {
    if (cellInfo.value == null || /["'].*[<>].*["']/.test(cellInfo.value)) {
        return cellInfo.value;
    } else {
        var code = cellInfo.value;
        code = code.replace(/(["](.*?)["])/g, "<span class = \"string_code\">$1</span>");
        code = code.replace(/(['](.*?)['])/g, "<span class = 'string_code'>$1</span>");
        code = code.replace(/(\.*[\w.]+|`.+`)(?=\()/g, "<span class = 'fun_call_code'>$1</span>");
        code = code.replace(/(%.+%)/g, "<span class = 'fun_call_code'>$1</span>");
        code = code.replace(/(\w+:{3}|\w+:{2})/g, "<span class = 'namespace_code'>$1</span>");
        code = code.replace(/\b((?:TRUE|FALSE|T|F|NA|NA_character_|NA_integer_|NA_complex_|NA_real_|NULL))\b/g, "<span class = 'specials_code'>$1</span>");
        code = code.replace(/\b((?:if|else|repeat|while|for|in|next|break))\b/g, "<span class = 'keyword_code'>$1</span>");
        code = code.replace(/\b([-+]?(0x[\dA-Fa-f]+|\d*\.?\d+([Ee]-?\d+)?i?|Inf|NaN))\b/g, "<span class = 'number_code'>$1</span>");
        code = code.replace("#", "<span class = 'comment_code'>#</span>");
        code = code.replace("(", "<span class = 'bracket_code'>(</span>");
        code = code.replace(")", "<span class = 'bracket_code'>)</span>");
        code = code.replace("{", "<span class = 'brace_code'>{</span>");
        code = code.replace("}", "<span class = 'brace_code'>}</span>");
        code = code.replace("[", "<span class = 'select_code'>[</span>");
        code = code.replace("]", "<span class = 'select_code'>]</span>");
        code = code.replace("$", "<span class = 'select_code'>$</span>");
        return React.createElement("span", {
            dangerouslySetInnerHTML: { __html: code }
        });
    }
}
