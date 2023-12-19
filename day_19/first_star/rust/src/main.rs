fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let (rules, parts) = input_file.split_once("\n\n").unwrap();
    let workflows = rules.lines().map(|w| parse_workflow(w)).collect::<Vec<_>>();
    let parts = parts
        .lines()
        .map(|p| {
            let p = p
                .trim_start_matches('{')
                .trim_end_matches('}')
                .split(',')
                .map(|g| g.split_once('=').unwrap().1.parse::<u32>().unwrap())
                .collect::<Vec<_>>();
            Part {
                extreme: p[0],
                musical: p[1],
                aerodynamic: p[2],
                shiny: p[3],
            }
        })
        .collect::<Vec<_>>();
    let output = parts
        .iter()
        .filter(|p| resolve_part(&workflows, "in".to_string(), p))
        .map(|p| p.extreme + p.musical + p.aerodynamic + p.shiny)
        .sum::<u32>();
    println!("{}", output);
}

fn parse_workflow(rule: &str) -> Workflow {
    let (name, rule) = rule.split_once("{").unwrap();
    let rule = rule.trim_end_matches('}');
    let rules = rule.split(',').map(|r| parse_rule(r)).collect();
    Workflow {
        name: name.to_string(),
        rules,
    }
}

fn parse_rule(rule: &str) -> Rule {
    if let Some((action, result)) = rule.split_once(":") {
        if let Some((typ, value)) = action.split_once('>') {
            Rule {
                action: Action::Gt {
                    typ: match typ {
                        "x" => Type::Extreme,
                        "m" => Type::Musical,
                        "a" => Type::Aerodynamic,
                        "s" => Type::Shiny,
                        _ => panic!("Unknown type"),
                    },
                    value: value.parse().unwrap(),
                },
                result: match result {
                    "A" => Result::Accept,
                    "R" => Result::Reject,
                    x => Result::Forward(x.to_string()),
                },
            }
        } else if let Some((typ, value)) = action.split_once('<') {
            Rule {
                action: Action::Lt {
                    typ: match typ {
                        "x" => Type::Extreme,
                        "m" => Type::Musical,
                        "a" => Type::Aerodynamic,
                        "s" => Type::Shiny,
                        _ => panic!("Unknown type"),
                    },
                    value: value.parse().unwrap(),
                },
                result: match result {
                    "A" => Result::Accept,
                    "R" => Result::Reject,
                    x => Result::Forward(x.to_string()),
                },
            }
        } else {
            panic!("Unknown action {action}")
        }
    } else {
        Rule {
            action: Action::None,
            result: match rule {
                "A" => Result::Accept,
                "R" => Result::Reject,
                x => Result::Forward(x.to_string()),
            },
        }
    }
}

fn resolve_part(workflows: &[Workflow], workflow_name: String, part: &Part) -> bool {
    let workflow = workflows
        .iter()
        .find(|w| w.name == workflow_name)
        .expect(&format!("Workflow {} not found", workflow_name));
    for rule in &workflow.rules {
        if check_part(part, rule) {
            match &rule.result {
                Result::Forward(name) => {
                    return resolve_part(workflows, name.to_string(), part);
                }
                Result::Accept => {
                    return true;
                }
                Result::Reject => {
                    return false;
                }
            }
        }
    }
    panic!("No matching rule found for part {:?}", part);
}

fn check_part(part: &Part, rule: &Rule) -> bool {
    match &rule.action {
        Action::Lt { typ, value } => match typ {
            Type::Extreme => part.extreme < *value,
            Type::Musical => part.musical < *value,
            Type::Aerodynamic => part.aerodynamic < *value,
            Type::Shiny => part.shiny < *value,
        },
        Action::Gt { typ, value } => match typ {
            Type::Extreme => part.extreme > *value,
            Type::Musical => part.musical > *value,
            Type::Aerodynamic => part.aerodynamic > *value,
            Type::Shiny => part.shiny > *value,
        },
        Action::None => true,
    }
}

#[derive(Debug)]
struct Part {
    extreme: u32,
    musical: u32,
    aerodynamic: u32,
    shiny: u32,
}

#[derive(Debug)]
struct Workflow {
    name: String,
    rules: Vec<Rule>,
}

#[derive(Debug)]
struct Rule {
    action: Action,
    result: Result,
}

#[derive(Debug)]
enum Type {
    Extreme,
    Musical,
    Aerodynamic,
    Shiny,
}

#[derive(Debug)]
enum Action {
    Lt { typ: Type, value: u32 },
    Gt { typ: Type, value: u32 },
    None,
}

#[derive(Debug)]
enum Result {
    Forward(String),
    Accept,
    Reject,
}
