use std::collections::VecDeque;

fn main() {
    let input = "../../input.txt";
    let input_file = std::fs::read_to_string(input).expect("Error reading input file");
    let (rules, _) = input_file.split_once("\n\n").unwrap();
    let workflows = rules.lines().map(|w| parse_workflow(w)).collect::<Vec<_>>();
    let part = Part {
        extreme: (1, 4000),
        musical: (1, 4000),
        aerodynamic: (1, 4000),
        shiny: (1, 4000),
    };
    let output = resolve_part(&workflows, "in".to_string(), part);
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

fn resolve_part(workflows: &[Workflow],initial: String, part: Part) -> i64 {
    let mut queue: VecDeque<(String, Part)> = VecDeque::new();
    queue.push_back((initial, part.clone()));
    let mut total_accepted = 0;

    while let Some((workflow_name, mut part_to_check)) = queue.pop_front() {
        let workflow = workflows
            .iter()
            .find(|w| w.name == workflow_name)
            .expect(&format!("Workflow {} not found", workflow_name));

        for rule in &workflow.rules {
            let mut parts = split_part(&part_to_check, rule).unwrap_or(vec![part_to_check.clone()]);
            for partt in parts.iter_mut() {
                if check_part(&partt, rule) {
                    match &rule.result {
                        Result::Forward(name) => {
                                queue.push_back((name.to_string(), partt.clone()));

                        }
                        Result::Accept => {
                            total_accepted += (partt.extreme.1 - partt.extreme.0 + 1)
                                * (partt.musical.1 - partt.musical.0 + 1)
                                * (partt.aerodynamic.1 - partt.aerodynamic.0 + 1)
                                * (partt.shiny.1 - partt.shiny.0 + 1);
                        }
                        Result::Reject => {
                            continue;
                        }
                    }
                }else{
                    part_to_check = partt.clone();
                }
            }
        }
    }

    total_accepted
}

fn split_part(part: &Part, rule: &Rule) -> Option<Vec<Part>> {
    match &rule.action {
        Action::Lt { typ, value } => {
            let (lo, hi) = match typ {
                Type::Extreme => part.extreme,
                Type::Musical => part.musical,
                Type::Aerodynamic => part.aerodynamic,
                Type::Shiny => part.shiny,
            };
            if *value <= lo {
                None
            } else if *value < hi {
                Some(vec![
                    Part {
                        extreme: if let Type::Extreme = typ { (lo, *value - 1) } else { part.extreme },
                        musical: if let Type::Musical = typ { (lo, *value - 1) } else { part.musical },
                        aerodynamic: if let Type::Aerodynamic = typ { (lo, *value - 1) } else { part.aerodynamic },
                        shiny: if let Type::Shiny = typ { (lo, *value - 1) } else { part.shiny },
                    },
                    Part {
                        extreme: if let Type::Extreme = typ { (*value, hi) } else { part.extreme },
                        musical: if let Type::Musical = typ { (*value, hi) } else { part.musical },
                        aerodynamic: if let Type::Aerodynamic = typ { (*value, hi) } else { part.aerodynamic },
                        shiny: if let Type::Shiny = typ { (*value, hi) } else { part.shiny },
                    },
                ])
            } else {
                None
            }
        }
        Action::Gt { typ, value } => {
            let (lo, hi) = match typ {
                Type::Extreme => part.extreme,
                Type::Musical => part.musical,
                Type::Aerodynamic => part.aerodynamic,
                Type::Shiny => part.shiny,
            };
            if *value >= hi {
                None
            } else if *value > lo {
                Some(vec![
                    Part {
                        extreme: if let Type::Extreme = typ { (lo, *value) } else { part.extreme },
                        musical: if let Type::Musical = typ { (lo, *value) } else { part.musical },
                        aerodynamic: if let Type::Aerodynamic = typ { (lo, *value) } else { part.aerodynamic },
                        shiny: if let Type::Shiny = typ { (lo, *value) } else { part.shiny },
                    },
                    Part {
                        extreme: if let Type::Extreme = typ { (*value + 1, hi) } else { part.extreme },
                        musical: if let Type::Musical = typ { (*value + 1, hi) } else { part.musical },
                        aerodynamic: if let Type::Aerodynamic = typ { (*value + 1, hi) } else { part.aerodynamic },
                        shiny: if let Type::Shiny = typ { (*value + 1, hi) } else { part.shiny },
                    },
                ])
            } else {
                None
            }
        }
        Action::None => None,
    }
}

fn check_part(part: &Part, rule: &Rule) -> bool {
    match &rule.action {
        Action::Lt { typ, value } => {
            let (lo, hi) = match typ {
                Type::Extreme => part.extreme,
                Type::Musical => part.musical,
                Type::Aerodynamic => part.aerodynamic,
                Type::Shiny => part.shiny,
            };
            if *value <= lo {
                return false;
            }
            if *value > hi {
                return true;
            }
            true
        }
        Action::Gt { typ, value } => {
            let (lo, hi) = match typ {
                Type::Extreme => part.extreme,
                Type::Musical => part.musical,
                Type::Aerodynamic => part.aerodynamic,
                Type::Shiny => part.shiny,
            };
            if *value >= hi {
                return false;
            }
            if *value < lo {
                return true;
            }
            true
        }
        Action::None => true,
    }
}

#[derive(Debug, Clone)]
struct Part {
    extreme: (i64, i64),
    musical: (i64, i64),
    aerodynamic: (i64, i64),
    shiny: (i64, i64),
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
    Lt { typ: Type, value: i64 },
    Gt { typ: Type, value: i64 },
    None,
}

#[derive(Debug)]
enum Result {
    Forward(String),
    Accept,
    Reject,
}